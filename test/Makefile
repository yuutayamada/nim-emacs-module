# !! Set EMACS_MODULE_DIR to the directory containing the emacs-module.h !!
EMACS_MODULE_DIR ?= $(shell dirname `which emacs`)
EMACS ?= emacs

NIM_CFLAGS = --passC:-I$(EMACS_MODULE_DIR) --passC:-std=gnu99

.PHONY: test

all: sample.so

sample.so: sample.nim
	nim c --out:sample.so --app:lib $(NIM_CFLAGS) $<

# nim c --out:sample.so --import:../emacs_module.nim --import:../emextra.nim --app:lib $(NIM_CFLAGS) $<

test:
	$(EMACS) -Q -L . $(LOADPATH) -l test.el -f ert-run-tests-batch-and-exit

clean:
	rm -fr nimcache sample.so
