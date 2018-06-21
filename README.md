# What is this?
This is a wrapper library to use Emacs Dynamic Module feature from
Nim language.

Note that the Emacs Dynamic Module feature is supported from Emacs
25.1 onwards.

## Note
I'm either Nim and C language's newbie, so probably I'm doing
something wrong... So beware. (PRs are welcome!)

## Usage Example

1. `git clone https://github.com/yuutayamada/nim-emacs-module`
2. `cd test`
3. `make EMACS_MODULE_DIR=../include/emacs25/` (this works on any Emacs version, 27.x too.)

If you don't have Emacs installed and want to generate just the
`sample.so`, do:

```
make sample.so EMACS_MODULE_DIR=../include/emacs25/
```

### Output


```
emacs --batch -L .  -l test.el -f ert-run-tests-batch-and-exit
Running 6 tests (2018-06-21 15:27:04-0400, selector ‘t’)
   passed  1/6  sample-mod-test-non-local-exit-signal-test (0.055210 sec)
   passed  2/6  sample-mod-test-non-local-exit-throw-test (0.000234 sec)
   passed  3/6  sample-mod-test-return-t (0.000264 sec)
   passed  4/6  sample-mod-test-return-uname-cmd (0.000247 sec)
   passed  5/6  sample-mod-test-sum (0.000267 sec)
   passed  6/6  sample-mod-test-vector-test (0.001737 sec)

Ran 6 tests, 6 results as expected (2018-06-21 15:27:04-0400, 0.058906 sec)
```

## Another Example

This example shows how simple it is to write a Nim proc with the extra
functionality as that of the `mymod_test` function in the [Emacs
Modules tutorial][diobla].

**Spoiler**: Other than few lines of boilerplate code, all you do is:

```nim
emacs.defun(return42, 0):
  env.make_integer(env, 42)
```

[Full code][return42]

Assuming that you already are past Steps 1 and 2 above, do:

```
make return42 EMACS_MODULE_DIR=../include/emacs25/
```

### Output

```
emacs --batch -L .  -l test-return42.el -f ert-run-tests-batch-and-exit
Running 1 tests (2018-06-21 16:48:28-0400, selector ‘t’)
   passed  1/1  return42-return42-cmd (0.000421 sec)

Ran 1 tests, 1 results as expected (2018-06-21 16:48:28-0400, 0.000766 sec)
```

## Other References
- [Introduction to Emacs modules][diobla]
- [emacs-mruby-test](https://github.com/syohex/emacs-mruby-test)
- M-x view-emacs-news and then look at `Emacs can now load shared/dynamic libraries (modules).` section
- modules directory of Emacs repository
- [Go + Emacs Modules](https://mrosset.github.io/emacs-module/)
- [GPL Compatible Licenses](https://www.gnu.org/licenses/license-list.html#GPLCompatibleLicenses)


[diobla]: http://diobla.info/blog-archive/modules-tut.html
[return42]: https://github.com/kaushalmodi/nim-emacs-module/blob/master/test/return42.nim
