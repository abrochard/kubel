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
             (lambda (cmd) "{
  \"clientVersion\": {
    \"major\": \"1\",
    \"minor\": \"28\",
    \"gitVersion\": \"v1.28.1\",
    \"gitCommit\": \"8dc49c4b984b897d423aab4971090e1879eb4f23\",
    \"gitTreeState\": \"archive\",
    \"buildDate\": \"1980-01-01T00:00:00Z\",
    \"goVersion\": \"go1.20.7\",
    \"compiler\": \"gc\",
    \"platform\": \"darwin/arm64\"
  },
  \"kustomizeVersion\": \"v5.0.4-0.20230601165947-6ce0bf390ce3\",
  \"serverVersion\": {
    \"major\": \"1\",
    \"minor\": \"27\",
    \"gitVersion\": \"v1.27.4+k3s1\",
    \"gitCommit\": \"36645e7311e9bdbbf2adb79ecd8bd68556bc86f6\",
    \"gitTreeState\": \"clean\",
    \"buildDate\": \"1970-01-01T01:01:01Z\",
    \"goVersion\": \"go1.20.7\",
    \"compiler\": \"gc\",
    \"platform\": \"linux/amd64\"
  }
}
")))
    (should (equal '(1 27) (kubel-kubernetes-version)))))

;; (ert "kubel--test-.*")

(provide 'kubel-test)
;;; kubel-test.el ends here
