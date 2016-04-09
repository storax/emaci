.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

test: export EMACI_TESTGITDIR=$(shell mktemp -d)
test: elpa
	echo created git temp dir $$EMACI_TESTGITDIR; \
  ./create_test_git_repo.sh
	$(CASK) exec ert-runner; \
	rm -rf $$EMACI_TESTGITDIR

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
