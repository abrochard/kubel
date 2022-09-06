;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-macs)

(ert-deftest kubel--test-kubernetes-compatible-p ()
  (cl-letf (((symbol-function 'kubel-kubernetes-version)
             (lambda () '(1 10 9))))
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

(ert-deftest kubel--test-kubernetes-version ()
  (setq kubel--kubernetes-version-cached nil)
  (cl-letf (((symbol-function 'kubel--exec-to-string)
             (lambda (cmd) "Client Version: version.Info{Major:\"1\", Minor:\"14\", GitVersion:\"v1.14.10\", GitCommit:\"575467a0eaf3ca1f20eb86215b3bde40a5ae617a\", GitTreeState:\"clean\", BuildDate:\"2019-12-11T12:41:00Z\", GoVersion:\"go1.12.12\", Compiler:\"gc\", Platform:\"darwin/amd64\"}
Server Version: version.Info{Major:\"1\", Minor:\"12\", GitVersion:\"v1.12.7\", GitCommit:\"6f482974b76db3f1e0f5d24605a9d1d38fad9a2b\", GitTreeState:\"clean\", BuildDate:\"2019-03-25T02:41:57Z\", GoVersion:\"go1.10.8\", Compiler:\"gc\", Platform:\"linux/amd64\"}")))
    (should (equal '(1 14 10) (kubel-kubernetes-version)))))

;; (ert "kubel--test-.*")

(provide 'kubel-test)
;;; kubel-test.el ends here
