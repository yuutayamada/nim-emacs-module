# This repository is still work in progress
I realized that I can't compile when I used args from emacs function...

# What is this?
This is a wrapper library to use Emacs' dynamic library feature from
Nim language.

Note that the Emacs' dynamic library feature is supported from Emacs
25.1, so you may need to switch `emacs-25` branch to enable the
feature.

## Note
I'm either Nim and C language's newbie, so probably I'm doing
something wrong... So beware. (PRs are welcome!)

## Usage
See sample.nim and Makefile

## Other References
- [Introduction to Emacs modules](http://diobla.info/blog-archive/modules-tut.html)
- [emacs-mruby-test](https://github.com/syohex/emacs-mruby-test)
- M-x view-emacs-news and then look at `Emacs can now load shared/dynamic libraries (modules).` section
