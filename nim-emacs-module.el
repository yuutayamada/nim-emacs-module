;;; nim-emacs-module.el --- Make Emacs functions by Nim -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/nim-emacs-module
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.0"))
;; Keywords: convenience, Nim

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;; Requirements:
;;
;; This package requires Emacs 25 or higher version because of using
;; Emacs’ dynamic module feature and you may need to compile your Emacs
;; with --with-modules option.  Also you need Nim compiler to compile
;; nim files.
;;
;; Usage:
;;
;; 1. Do M-x ‘nim-emacs-module-init’.
;;    It will create nim-emacs-module directory in your
;;    ‘user-emacs-directory’ and put a nim.cfg file (for Nim’s
;;    configuration file)
;; 2. Go to the nim-emacs-module directory.
;; 3. Make a nim file in the nim-emacs-module directory.
;; 4. Do M-x ‘nim-emacs-module-insert-template’
;; 5. Follow the inserted instruction or see the examples of test directory.
;; 6. Have fun :)
;;
;; Note that this package is still working in progress and I might change
;; some details.
;;
;; Other References:
;;  - [Introduction to Emacs modules](http://diobla.info/blog-archive/modules-tut.html)
;;  - [emacs-mruby-test](https://github.com/syohex/emacs-mruby-test)
;;  - M-x view-emacs-news and then look at `Emacs can now load
;;    shared/dynamic libraries (modules).` section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'subr-x)
(require 'let-alist)

(defvar nim-emacs-module-template
  "# `plugin_is_GPL_compatible` indicates that its code is
# released under the GPL or compatible license; Emacs will refuse to
# load modules that don't export such a symbol.
{.emit:\"int plugin_is_GPL_compatible;\".}


import emacs_module # Primitive wrapper for emacs_module.h
import emextra      # Helper library


# You can access following types:
#
#   env: ptr emacs_env
#   nargs: ptrdiff_t
#   args: ptr array[0..max_args, emacs_value]
#   data: pointer

init(emacs)

%s

# Provide functions registered by above from here.
emacs.provide()%s"
  "Template string.")

(defvar nim-emacs-module-template-example
  '((example . "
# Example(Create a file named `mod_test.nim`):
emacs.defun(return_t, 1):
  env.intern(env, \"t\".cstring)")
    (test . "

# How to test the example's code:
#
#   $ emacs -Q -L .
#
# and then in *scratch* buffer, evaluate those lines:
#
#   The file name ‘mod-test‘ is added as prefix name of each functions and
#   its package name.
#   (require '%s)
#   (mod-test-return-t)"))
  "Additional template.")

(defvar nim-emacs-module-dir
  (when-let ((pkg (locate-library "nim-emacs-module")))
    (file-name-directory pkg))
  "Directly where the nim-emacs-module.nim is placed.")

(defvar nim-emacs-module-module-h-dir
  (let ((dir (file-name-directory (executable-find "emacs"))))
    (if (file-exists-p (format "%semacs-module.h" dir))
        dir
      nil))
  "Directly of emacs-module.h.")


(defvar nim-emacs-module-dylib-dir
  (format "%snim-emacs-module/" user-emacs-directory)
  "The place where the .so files are placed.")

(defvar nim-emacs-module-cflags
  "--passC:-std=gnu99")

;;;###autoload
(defun nim-emacs-module-insert-template ()
  "Insert template for nim-emacs-module."
  (interactive)
  (let-alist nim-emacs-module-template-example
    (let ((example .example)
          (test (if (string< "" .test)
                    (format .test (file-name-base))
                  "")))
      (insert (format nim-emacs-module-template
                      example
                      (file-name-base)
                      test)))))

;;;###autoload
(defun nim-emacs-module-init ()
  "Prepare things for nim-emacs-module."
  (interactive)
  (nim-emacs-module--make-dir nim-emacs-module-dylib-dir)
  (let ((file (format "%snim.cfg" nim-emacs-module-dylib-dir)))
    (unless (file-exists-p file)
      (if (not (file-directory-p nim-emacs-module-dir))
          (error "Please set ‘nim-emacs-module-dir’")
        (save-current-buffer
          (with-temp-file file
            (insert (format "path: \"%s\"" nim-emacs-module-dir))))))))

(defun nim-emacs-module--assert (file)
  "Check some requirements, according to FILE."
  (cond
   ((not (file-exists-p file))
    (error "Error: %s doesn't exist" file))
   ((not (version< "25.0.0.0" emacs-version))
    (error "Error: need Emacs version higher than 25"))
   ((not (derived-mode-p 'nim-mode))
    (yes-or-no-p "major-mode isn’t nim-mode. Are you really sure?"))
   (t t)))

(defun nim-emacs-module--make-dir (directory)
  "Make DIRECTORY."
  (unless (file-directory-p directory)
    (make-directory directory)))

(defun nim-emacs-module--compile (file)
  "Compile a nim FILE to a .so/.dylib/.dll extension file."
  (when (nim-emacs-module--assert file)
    (let ((cmd (nim-emacs-module--get-command file)))
      (async-shell-command
       cmd (get-buffer-create "*nim-emacs-module-compile*")))))

(defun nim-emacs-module--get-command (file)
  "Return command using FILE."
  (let ((emacs-module-h (format "--passC:-I%s" nim-emacs-module-module-h-dir))
        (cflags nim-emacs-module-cflags)
        (base (file-name-base file)))
    (format "nim c --out:%s.%s --app:lib %s %s %s"
            base (nim-emacs-module--get-extension)
            cflags emacs-module-h file)))

(defun nim-emacs-module--get-extension ()
  "Return file extension."
  (cl-case system-type
    ;; I’m only testing on Linux, so please let me know
    ;; if it’s something wrong... (is the cygwin right place?)
    ((ms-dos windows-nt cygwin) "dll")
    (darwin "dylib")
    (t "so")))

;;;###autoload
(defun nim-emacs-module-compile ()
  "Compile current Nim file to a dylib file that can Emacs load."
  (interactive)
  (nim-emacs-module--make-dir nim-emacs-module-dylib-dir)
  (let ((default-directory nim-emacs-module-dylib-dir))
    (nim-emacs-module--compile buffer-file-name)))

;; Work with nim-mode

(defun nim-emacs-module-file-p ()
  "Return t if current file is inside ‘nim-emacs-module-dylib-dir’."
  (string-match nim-emacs-module-dylib-dir buffer-file-name))

;;;###autoload
(defun nim-compile-or-module-compile (&rest _r)
  "Work in progress."
  (interactive)
  (call-interactively
   (if (nim-emacs-module-file-p)
       'nim-emacs-module-compile
     'nim-compile)))

(defun nim-emacs-module-setup-nim-mode ()
  "Setup for nim-mode."
  (remove-hook 'nim-mode-hook 'nim-emacs-module-setup-nim-mode)
  (with-no-warnings
    (define-key nim-mode-map [remap nim-compile] 'nim-compile-or-module-compile)))

;;;###autoload
(with-eval-after-load "nim-mode"
  (add-hook 'nim-mode-hook 'nim-emacs-module-setup-nim-mode))

(provide 'nim-emacs-module)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; nim-emacs-module.el ends here
