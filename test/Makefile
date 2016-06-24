# Please set emacs/src directory to include emacs-module.h
EMACS_SRC ?= $(shell dirname `which emacs`)
EMACS ?= emacs

CFLAGS = --passC:-I$(EMACS_SRC) --passC:-std=gnu99

.PHONY: test

all: sample.so

sample.so: sample.nim
	nim c --out:sample.so --app:lib $(CFLAGS) $<

# nim c --out:sample.so --import:../emacs_module.nim --import:../emextra.nim --app:lib $(CFLAGS) $<

test:
	$(EMACS) -Q -L . $(LOADPATH) -l test.el -f ert-run-tests-batch-and-exit
