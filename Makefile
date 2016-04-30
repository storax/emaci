.PHONY : test

EMACS ?= emacs
CASK ?= cask

LOADPATH = -L .

ELPA_DIR = \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

test: export EMACI_TESTGITDIR=$(shell mktemp -d)
test: export EMACI_SAVEDIR=$(shell mktemp -d)
test: export EMACI_TESTGITDIR2=$(shell mktemp -d)
test: elpa
	echo created git temp dir $$EMACI_TESTGITDIR; \
  ./create_test_git_repo.sh
	$(CASK) exec ert-runner; \
	rm -rf $$EMACI_TESTGITDIR; \
	rm -rf $$EMACI_TESTGITDIR2; \
  rm -rf $$EMACI_SAVEDIR

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@
