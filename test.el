;;; test.el --- test for this repository -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>

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
;;; Commentary:

;;; Code:

(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))
(require 'libsample)

(ert-deftest mod-test-return-t ()
  (should (eq t (mod-test-return-t 0)))
  (should (eq t (mod-test-return-t "abc")))
  (should (eq t (mod-test-return-t t)))
  (should (eq t (mod-test-return-t nil)))
  (should (eq t (mod-test-return-t ?a)))
  (should (string= "uname -a" (mod-test-return-uname-cmd "-a")))
  (should (eq 156 (mod-test-return-156 ""))))

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile: t
;; End:

;;; test.el ends here
