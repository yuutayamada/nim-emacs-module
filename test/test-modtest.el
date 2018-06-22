;;; test-modtest.el --- test modtest.nim -*- lexical-binding: t; -*-

;; Author: Kaushal Modi <kaushal.modi@gmail.com>

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

(require 'modtest)


;; TODO: It's not clear how to test if `modtest-globref-make' is doing
;; the right thing.
(ert-deftest modtest-globref-make ()
  (let ((refstr ""))
    (dotimes (i 100)
      (setq refstr (concat refstr "abcdefghijklmnopqrstuvwxyz")))
    (should (string= refstr (modtest-globref-make)))))

(ert-deftest modtest-non-local-exit-signal-test ()
  (should-error (modtest-signal)))

(ert-deftest modtest-non-local-exit-throw-test ()
  (should (equal
           (catch 'tag
             (modtest-throw)
             (ert-fail "expected throw"))
           42)))

(ert-deftest modtest-make-string ()
  (should (string= "--" (modtest-make-string 2 ?-)))
  (should (string= "aaaaa" (modtest-make-string 5 ?a)))
  (should (string= "" (modtest-make-string 0 ?a))))

(ert-deftest modtest-return-t ()
  (should (eq t (modtest-return-t 0)))
  (should (eq t (modtest-return-t "abc")))
  (should (eq t (modtest-return-t t)))
  (should (eq t (modtest-return-t nil)))
  (should (eq t (modtest-return-t ?a))))

(ert-deftest modtest-get-type ()
  (should (eq 'string (modtest-get-type "abc")))
  (should (eq 'integer (modtest-get-type 42)))
  (should (eq 'float (modtest-get-type 42.0)))
  (should (eq 'symbol (modtest-get-type nil)))
  (should (eq 'symbol (modtest-get-type t)))
  (should (eq 'symbol (modtest-get-type '())))
  (should (eq 'cons (modtest-get-type (cons 1 2))))
  (should (eq 'cons (modtest-get-type '(1 . 2))))
  (should (eq 'cons (modtest-get-type '(1 2 3)))) ;Interestingly, this is a "cons" too.
  (should (eq 'cons (modtest-get-type (list 1 2 3))))) ;.. and this too!

(ert-deftest modtest-is-true ()
  (should (eq nil (modtest-is-true nil)))
  (should (eq nil (modtest-is-true '())))
  (should (eq nil (modtest-is-true ())))
  (should (eq nil (modtest-is-true (not t))))
  (should (eq t (modtest-is-true "abc")))
  (should (eq t (modtest-is-true "")))
  (should (eq t (modtest-is-true 42)))
  (should (eq t (modtest-is-true 42.0)))
  (should (eq t (modtest-is-true t)))
  (should (eq t (modtest-is-true (cons 1 2))))
  (should (eq t (modtest-is-true '(1 . 2))))
  (should (eq t (modtest-is-true '(1 2 3))))
  (should (eq t (modtest-is-true (list 1 2 3)))))

(ert-deftest modtest-eq ()
  (should (eq t (modtest-eq nil nil)))
  (should (eq t (modtest-eq t t)))
  (should (eq nil (modtest-eq nil t)))
  (should (eq nil (modtest-eq "abc" "abc"))) ;These are *not* the same Lisp objects!
  (should (eq nil (modtest-eq "" nil)))
  (should (eq t (modtest-eq 42 42)))
  (should (eq t (modtest-eq ?a ?a)))
  (should (eq nil (modtest-eq 42 43)))
  (should (eq nil (modtest-eq 42.0 42.0))) ;These are *not* the same Lisp objects!
  (should (eq nil (modtest-eq (cons 1 2) (cons 1 2))))) ;These are *not* the same Lisp objects!

(ert-deftest modtest-sum ()
  (should (equal 10 (modtest-sum 3 7)))
  (should-error (modtest-sum 3) :type 'wrong-number-of-arguments)
  (should-error (modtest-sum "1" 2) :type 'wrong-type-argument)
  (should-error (modtest-sum 2 "1") :type 'wrong-type-argument)
  (should-error (modtest-sum 2.0 1.0) :type 'wrong-type-argument))

(ert-deftest modtest-sum-float ()
  (should (equal 10.0 (modtest-sum-float 3.3 6.7)))
  (should-error (modtest-sum-float 3.0) :type 'wrong-number-of-arguments)
  (should-error (modtest-sum-float "1" 2) :type 'wrong-type-argument)
  (should-error (modtest-sum-float 2 "1") :type 'wrong-type-argument)
  (should-error (modtest-sum-float 2 1) :type 'wrong-type-argument))

(ert-deftest modtest-string ()
  (should (string= "The quick brown fox jumped over the lazy dog." (modtest-lazy)))
  (should (string= "Hello World" (modtest-hello "World"))))

(ert-deftest modtest-uname ()
  (let ((ref-uname-a-output (progn
                              (require 'subr-x)
                              (string-trim (shell-command-to-string "uname -a")))))
    (should (string= ref-uname-a-output (modtest-uname "-a")))))

(ert-deftest modtest-vector-test ()
  (dolist (s '(2 10 100 1000))
    (dolist (e '(42 foo "foo" 3.14))
      (let* ((v-ref (make-vector 2 e))
             (eq-ref (eq (aref v-ref 0) (aref v-ref 1)))
             (v-test (make-vector s nil)))

        (should (eq (modtest-vector-fill v-test e) t))
        (should (eq (modtest-vector-eq v-test e) eq-ref))))))


;;; test-modtest.el ends here
