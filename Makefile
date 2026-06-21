.PHONY: compile autoloads test check-declare check-declare-local clean

EMACS ?= emacs
# fzf-native is not on MELPA; default to a sibling checkout.  Override
# from the command line if your layout differs:
#   make test FZF_NATIVE_DIR=/path/to/fzf-native
FZF_NATIVE_DIR ?= ../fzf-native

# Optional Elisp deps consulted by `check-declare' to verify the
# forward declarations against upstream APIs.  Default to sibling
# checkouts (the layout CI uses); override locally to point at your
# elpa install or any other clone:
#   make check-declare IVY_DIR=~/.emacs.d/elpa/ivy-NNN
IVY_DIR ?= ../swiper
PROJECTILE_DIR ?= ../projectile
VERTICO_DIR ?= ../vertico

PACKAGE := fzfa
AUTOLOADS := $(PACKAGE)-autoloads.el

# Every .el we ship except tests, the package descriptor, and the
# generated autoloads file itself.  `fzfa.el' is listed first so its
# byte-compile warnings surface before the extension files' warnings —
# the extensions all `(require 'fzfa)' and otherwise mask problems in
# the core during a noisy build.
SRC := $(PACKAGE).el \
       $(filter-out $(PACKAGE).el $(AUTOLOADS) $(PACKAGE)-pkg.el \
                    $(PACKAGE)-test.el, \
                    $(wildcard *.el))

compile: clean autoloads
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(SRC)

autoloads:
	$(EMACS) -Q --batch \
	  --eval "(loaddefs-generate default-directory \"$(AUTOLOADS)\" nil \"(add-to-list 'load-path (or (and load-file-name (file-name-directory load-file-name)) (car load-path)))\n\")"

# Loads the fzf-native dynamic module before running tests.  Existing
# tests are pure-Elisp helpers and would pass without it, but loading
# it in CI catches "missing binary / wrong arch / module load fails"
# regressions and lets future async-path tests just work.
#
# Depends on `compile' so any byte-compile warning fails the build
# before tests run.
test: compile
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  -l fzf-native \
	  -f fzf-native-load-dyn \
	  -l ert \
	  -l ./fzfa-test.el \
	  -f ert-run-tests-batch-and-exit

# Verify `declare-function' forward declarations against the source
# files they name.  Catches upstream renames / arglist drift at test
# time — the trade-off for using forward declarations instead of
# hard `require's.  Functions defined in C (fzf-native) use the
# `ext:fzf-native-module' marker so the verifier skips the missing-
# Elisp-source lookup cleanly; optional Elisp deps must be reachable
# via the *_DIR vars above for their APIs to be verified.
check-declare:
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  -L $(IVY_DIR) -L $(PROJECTILE_DIR) -L $(VERTICO_DIR) \
	  --eval "(require 'check-declare)" \
	  --eval "(let ((errs (check-declare-file \"$(PACKAGE).el\"))) \
	             (when errs (kill-emacs 1)))"

# Local variant of `check-declare' for developers running against
# their own installed packages.  Auto-detects ELPA_DIR based on the
# current `$(EMACS)' major version (matches the common per-Emacs-
# version layout `~/.emacs.d/elpa/30/').  Override if you don't use
# that layout:
#   make check-declare-local ELPA_DIR=~/.emacs.d/elpa
ELPA_DIR ?= $(HOME)/.emacs.d/elpa/$(shell $(EMACS) -Q --batch --eval "(princ emacs-major-version)" 2>/dev/null)

check-declare-local:
	@if [ ! -d "$(ELPA_DIR)" ]; then \
	  echo "ELPA_DIR=$(ELPA_DIR) does not exist; override with"; \
	  echo "  make check-declare-local ELPA_DIR=/path/to/elpa"; \
	  exit 1; \
	fi
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  --eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
	  --eval "(package-initialize)" \
	  --eval "(require 'check-declare)" \
	  --eval "(let ((errs (check-declare-file \"$(PACKAGE).el\")) real) \
	             (dolist (group errs) \
	               (dolist (e (cdr group)) \
	                 (unless (equal (nth 2 e) \"file not found\") \
	                   (push e real)))) \
	             (when real (kill-emacs 1)))"

clean:
	rm -f *.elc $(AUTOLOADS)
