.PHONY: compile autoloads test clean

EMACS ?= emacs
# fzf-native is not on MELPA; default to a sibling checkout.  Override
# from the command line if your layout differs:
#   make test FZF_NATIVE_DIR=/path/to/fzf-native
FZF_NATIVE_DIR ?= ../fzf-native

PACKAGE := fzfa
AUTOLOADS := $(PACKAGE)-autoloads.el

# Every .el we ship except tests, the package descriptor, and the
# generated autoloads file itself.
SRC := $(filter-out $(AUTOLOADS) $(PACKAGE)-pkg.el $(PACKAGE)-test.el, \
                    $(wildcard *.el))

compile: autoloads
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  -f batch-byte-compile $(SRC)

autoloads:
	$(EMACS) -Q --batch \
	  --eval "(loaddefs-generate default-directory \"$(AUTOLOADS)\")"

# Loads the fzf-native dynamic module before running tests.  Existing
# tests are pure-Elisp helpers and would pass without it, but loading
# it in CI catches "missing binary / wrong arch / module load fails"
# regressions and lets future async-path tests just work.
test:
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  -l fzf-native \
	  -f fzf-native-load-dyn \
	  -l ert \
	  -l ./fzfa-test.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc $(AUTOLOADS)
