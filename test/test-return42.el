(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))

(require 'return42)

(ert-deftest return42-return42-cmd ()
  (should (= 42 (return42-return42))))
