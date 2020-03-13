;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kubel--test-kubernetes-compatible-p ()
  (flet ((kubel-kubernetes-version () '(1 10 9)))
    (should (kubel-kubernetes-compatible-p '(1 9 0)))
    (should (kubel-kubernetes-compatible-p '(0 11 0)))
    (should (not (kubel-kubernetes-compatible-p '(1 11 0))))
    (should (not (kubel-kubernetes-compatible-p '(2 0 25))))))

(ert-deftest kubel--test-extract-value ()
  (let ((line "0123456789"))
    (should (equal "0" (kubel--extract-value line 0 1)))
    (should (equal "0123" (kubel--extract-value line 0 4)))
    (should (equal "34567" (kubel--extract-value line 3 8)))
    (should (equal "56789" (kubel--extract-value line 5 "end")))
    (should (equal "-" (kubel--extract-value "     " 0 5)))
    (should (equal "-" (kubel--extract-value "  " 0 "end")))))

;; (ert "kubel--test-.*")

(provide 'kubel-test)
;;; kubel-test.el ends here
