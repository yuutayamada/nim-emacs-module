# Please set emacs/src directory to include emacs-module.h
EMACS_SRC ?= $(shell dirname `which emacs`)
EMACS ?= emacs

CFLAGS = --passC:-I$(EMACS_SRC) --passC:-std=gnu99

.PHONY: test

all: libsample.so

libsample.so: sample.nim
	nim c --app:lib $(CFLAGS) $<

test:
	$(EMACS) -Q -L . $(LOADPATH) -l test.el -f ert-run-tests-batch-and-exit
