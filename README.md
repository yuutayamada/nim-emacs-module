# What is this?
This is a wrapper library to use Emacs Dynamic Module feature from
Nim language.

Note that the Emacs Dynamic Module feature is supported from Emacs
25.1 onwards.

## Note
I'm either Nim and C language's newbie, so probably I'm doing
something wrong... So beware. (PRs are welcome!)

## Usage

1. `git clone https://github.com/kaushalmodi/nim-emacs-module`
2. `cd test`
3. `make EMACS_MODULE_DIR=/path/containing/emacs-module.h`

## Other References
- [Introduction to Emacs modules](http://diobla.info/blog-archive/modules-tut.html)
- [emacs-mruby-test](https://github.com/syohex/emacs-mruby-test)
- M-x view-emacs-news and then look at `Emacs can now load shared/dynamic libraries (modules).` section
- modules directory of Emacs repository
- [Go + Emacs Modules](https://mrosset.github.io/emacs-module/)
- [GPL Compatible Licenses](https://www.gnu.org/licenses/license-list.html#GPLCompatibleLicenses)
