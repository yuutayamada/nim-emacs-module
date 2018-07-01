# !! Set EMACS_MODULE_DIR to the directory containing the emacs-module.h !!
EMACS_MODULE_DIR ?= $(shell dirname `which emacs`)
EMACS ?= emacs

NIM_CFLAGS = --passC:-I$(EMACS_MODULE_DIR) --passC:-std=gnu99

.PHONY: return42 test_return42 sample test_sample modtest test_modtest clean test all


## return42
return42: clean return42.so test_return42

return42.so: return42.nim
	nim c --out:return42.so --app:lib $(NIM_CFLAGS) $<

test_return42:
	$(EMACS) --batch -L . $(LOADPATH) -l test-return42.el -f ert-run-tests-batch-and-exit


## sample
sample: clean sample.so test_sample

sample.so: sample.nim
	nim c --out:sample.so --app:lib $(NIM_CFLAGS) $<

test_sample:
	$(EMACS) --batch -L . $(LOADPATH) -l test-sample.el -f ert-run-tests-batch-and-exit


## modtest
modtest: clean modtest.so test_modtest

modtest.so: modtest.nim
	nim c --out:modtest.so --app:lib $(NIM_CFLAGS) $<

test_modtest:
	$(EMACS) --batch -L . $(LOADPATH) -l test-modtest.el -f ert-run-tests-batch-and-exit


clean:
	rm -fr nimcache *.so

test: test_return42 test_sample test_modtest

all: return42 sample modtest
