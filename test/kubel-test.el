;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-macs)
(require 'kubel)

(ert-deftest kubel--test-extract-value ()
  (let ((line "0123456789"))
    (should (equal "0" (kubel--extract-value line 0 1)))
    (should (equal "0123" (kubel--extract-value line 0 4)))
    (should (equal "34567" (kubel--extract-value line 3 8)))
    (should (equal "56789" (kubel--extract-value line 5 "end")))
    (should (equal "-" (kubel--extract-value "     " 0 5)))
    (should (equal "-" (kubel--extract-value "  " 0 "end")))))

(ert-deftest kubel--test-prefix-scopes-selector-to-list-commands ()
  "The label selector must not leak into single-resource commands.

kubectl refuses a resource name combined with `--selector', and `exec'
does not accept `--selector' at all."
  (let ((kubel-kubectl "kubectl")
        (kubel-context "ctx")
        (kubel-namespace "ns")
        (kubel-selector "app=foo"))
    (should (equal "kubectl --context ctx -n ns"
                   (kubel--get-command-prefix)))
    (should (equal "kubectl --context ctx -n ns --selector app=foo"
                   (kubel--get-list-command-prefix)))))

;; (ert "kubel--test-.*")

(provide 'kubel-test)
;;; kubel-test.el ends here
