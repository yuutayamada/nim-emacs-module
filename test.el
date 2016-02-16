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
  (should (eq t (mod-test-return-t ?a))))

(ert-deftest mod-test-return-uname-cmd ()
  (should (string= "uname -a" (mod-test-return-uname-cmd "-a"))))

(ert-deftest mod-test-sum ()
  (should (eq 10 (mod-test-sum 3 7)))
  (should-error (mod-test-sum "1" 2) :type 'wrong-type-argument)
  (should-error (mod-test-sum 2 "1") :type 'wrong-type-argument))

(ert-deftest mod-test-vector-test ()
  (dolist (s '(2 10 100 1000))
    (dolist (e '(42 foo "foo" 3.14))
      (let* ((v-ref (make-vector 2 e))
             (eq-ref (eq (aref v-ref 0) (aref v-ref 1)))
             (v-test (make-vector s nil)))

        (should (eq (mod-test-vector-fill v-test e) t))
        (should (eq (mod-test-vector-eq v-test e) eq-ref))))))

(ert-deftest mod-test-non-local-exit-signal-test ()
  (should-error (mod-test-signal)))

(ert-deftest mod-test-non-local-exit-throw-test ()
  (should (equal
           (catch 'tag
             (mod-test-throw)
             (ert-fail "expected throw"))
           42)))

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile: t
;; End:

;;; test.el ends here
