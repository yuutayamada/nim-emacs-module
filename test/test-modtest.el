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


(ert-deftest modtest-non-local-exit-signal-test ()
  (should-error (modtest-signal)))

(ert-deftest modtest-non-local-exit-throw-test ()
  (should (equal
           (catch 'tag
             (modtest-throw)
             (ert-fail "expected throw"))
           42)))

(ert-deftest modtest-return-t ()
  (should (eq t (modtest-return-t 0)))
  (should (eq t (modtest-return-t "abc")))
  (should (eq t (modtest-return-t t)))
  (should (eq t (modtest-return-t nil)))
  (should (eq t (modtest-return-t ?a))))

(ert-deftest modtest-sum ()
  (should (eq 10 (modtest-sum 3 7)))
  (should-error (modtest-sum "1" 2) :type 'wrong-type-argument)
  (should-error (modtest-sum 2 "1") :type 'wrong-type-argument))

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
