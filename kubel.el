;;; kubel.el --- extension for controlling Kubernetes with limited permissions -*- lexical-binding: t; -*-

;; Copyright (C) 2018, Adrien Brochard

;; This file is NOT part of Emacs.

;; This  program is  free  software; you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

;; You should have  received a copy of the GNU  General Public License
;; along  with  this program;  if  not,  write  to the  Free  Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;; Version: 1.0
;; Author: Adrien Brochard
;; Keywords: kubernetes k8s tools processes
;; URL: https://github.com/abrochard/kubel
;; License: GNU General Public License >= 3
;; Package-Requires: ((transient "0.1.0") (emacs "25.3"))

;;; Commentary:

;; Emacs extension for controlling Kubernetes with limited permissions.
;; Mostly focuses on pod management for now.

;;; Usage:

;; To list the pods in your current context and namespace, call
;;
;; M-x kubel
;;;
;; To set said namespace and context, respectively call
;;
;; M-x kubel-set-namespace
;; M-x kubel-set-context
;;
;; Note that namespace will autocomplete but not context,
;; this is because I interact with kubernetes through a user who
;; does not have permissions to list namespaces.

;;; Shortcuts:

;; On the kubel screen, place your cursor on the pod
;;
;; enter => get pod details
;; h => help popup
;; C => set context
;; n => set namespace
;; g => refresh pods
;; p => port forward pod
;; e => exec into pod
;; d => describe popup
;; l => log popup
;; c => copy popup
;; k => delete pod
;; j => jab deployment to force rolling update
;; f => set a substring filter for pod name
;; r => see the rollout history for resource
;;

;;; Customize:

;; By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.

;;; Code:

(require 'transient)

(defvar kubel--list-format
  [("Name" 50 t)
   ("Ready" 10 t)
   ("Status" 20 t)
   ("Restarts" 10 t)
   ("Age" 15 t)]
  "List format.")

(defconst kubel--list-sort-key
  '("NAME" . nil)
  "Sort table on this key.")

(defconst kubel--status-colors
  '(("Running" . "green")
    ("Healthy" . "green")
    ("Active" . "green")
    ("Ready" . "green")
    ("Error" . "red")
    ("MemoryPressure" . "red")
    ("PIDPressure" . "red")
    ("DiskPressure" . "red")
    ("NetworkUnavailable" . "red")
    ("Completed" . "yellow")
    ("CrashLoopBackOff" . "red")
    ("Terminating" . "blue"))
  "Associative list of status to color.")

(defvar kubel-namespace ""
  "Current namespace.")

(defvar kubel-resource "pods"
  "Current resource.")

(defvar kubel-context
  (replace-regexp-in-string
   "\n" "" (shell-command-to-string "kubectl config current-context"))
  "Current context.  Tries to smart default.")

(defvar kubel-pod-filter ""
  "Substring filter for pod name.")

(defvar kubel-namespace-history '()
  "List of previously used namespaces.")

(defvar kubel-log-tail-n "100"
  "Number of lines to tail.")

(defvar kubel-kubernetes-resources-list '("Pods"
					  "Services"
					  "Namespaces"
					  "Nodes"
					  "Configmaps"
					  "Secrets"
					  "Bindings"
					  "PersistentVolumeClaims"
					  "PersistentVolumes"
					  "ReplicationControllers"
					  "ResourceQuotas"
					  "ServiceAccounts"
					  "Deployments"
					  "DaemonSets"
					  "ReplicaSets"
					  "StatefulSets"
					  "Jobs"
					  "Images"
					  "Ingresses"
					  "ClusterRoles"
					  "RoleBindings"
					  "Roles"
					  ))

(defun kubel-kubernetes-version ()
  "Return a list with (major-version minor-version patch)."
  (let ((version-string (shell-command-to-string "kubectl version")))
    (string-match "GitVersion:\"v\\([0-9]*\\)\.\\([0-9]*\\)\.\\([0-9]*\\)\"" version-string)
    (list
     (string-to-number (match-string 1 version-string))
     (string-to-number (match-string 2 version-string))
     (string-to-number (match-string 3 version-string)))
  )
)

(defun kubel-kubernetes-compatible-p (version)
  "Return TRUE if kubernetes version is greater than or equal to VERSION.
VERSION should be a list of (major-version minor-version patch)."
  (let*
      ((kubernetes-version (kubel-kubernetes-version))
       (kubernetes-major-version (nth 0 kubernetes-version))
       (kubernetes-minor-version (nth 1 kubernetes-version))
       (kubernetes-patch-version (nth 2 kubernetes-version))

       )
    (or
     (< (nth 0 version) kubernetes-major-version)
     (< (nth 1 version) kubernetes-minor-version)
     (< (nth 2 version) kubernetes-patch-version)
     )

    )
  )


(defun kubel--populate-list ()
  "Return a list with a tabulated list format and tabulated-list-entries."
  (let  ((body (shell-command-to-string (concat (kubel--get-command-prefix) " get " kubel-resource)))
	 )
    (if (s-starts-with? "No resources found" body)
	(message "No resources found")
      )
    (list (kubel--get-list-format body) (nbutlast (kubel--get-list-entries body) ))
    )
  )

(defun kubel--column-entry (temp-file)
  "Define column properties for tabulated-list-format."
  (lexical-let ((temp-file temp-file))
    (function
     (lambda (colnum)
       (list (kubel--column-header temp-file colnum) (kubel--column-width temp-file colnum) t)
       )

     )

    )
  )


(defun kubel--get-list-format (body)
  "Generate tabulated-list-format based on kubectl header row."
  (let* ((temp-file (make-temp-file "kubel-" nil nil body)))
    (defun kubel--get-column-entry (colnum)
      (let ((kubel--get-entry (kubel--column-entry temp-file)))
	(funcall kubel--get-entry colnum)
	)

      )


    (cl-map 'vector #'kubel--get-column-entry (number-sequence 1 (kubel--ncols temp-file)) )
    )
  )

(defun kubel--get-list-entries (body)
  "Generate a list of entries for tabulated-list-mode."
  (let ((entrylist (vector)))
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (forward-line)
      (setq morelines 1)
      (while morelines
	(beginning-of-line)
	(setq firstchar (point))
	(end-of-line)
	(setq lastchar (point))
	(setq theline (mapcar 'kubel--propertize-status (split-string (buffer-substring firstchar lastchar) ) ))
	;; TODO Modify theline to apply colourisation
	(setq entrylist (append entrylist (list (list (car theline) (vconcat [] theline)))))
	(setq morelines (= 0 (forward-line 1)))
	)

      )
    entrylist
    )   ; (vector (split-string ((temp-file))) )
  )


(defun kubel--ncols (temp-file)
  "Return number of columns in kubectl output."
  (string-to-number (shell-command-to-string (concat "awk 'END{print NF}' " temp-file)) )
  )

(defun kubel--nrows (temp-file)
  "Return number of rows in kubectl output."
  (string-to-number (shell-command-to-string (concat "awk 'END{print NR}' " temp-file)) )
  )


(defun kubel--column-header (temp-file colnum)
  "Return header of column colnum in kubeclt output."
  (replace-regexp-in-string "\n" "" (shell-command-to-string (concat "awk 'NR==1 {print $" (number-to-string colnum) "}' " temp-file)) )
  )

(defun kubel--column-width (temp-file colnum)
  "Return width of column colnum in kubectl output."
  (+ 4 (string-to-number (shell-command-to-string (concat "awk 'BEGIN{maxlen = 0}{if(length($" (number-to-string colnum)
							  ") > maxlen){maxlen = length($" (number-to-string colnum )
							  ")}}END{print maxlen}' " temp-file))
			 ) ))


(defun kubel--buffer-name ()
  "Return kubel buffer name."
  (concat "*kubel (" kubel-namespace ") [" kubel-context "]*"))

;; (defun kubel--extract-pod-line ()
;;   "Return a vector from the pod line."
;;   (let ((name (match-string 1))
;; 	(ready (match-string 2))
;; 	(status (match-string 3))
;; 	(restarts (match-string 4))
;; 	(age (match-string 5)))
;;     (vector (kubel--propertize-pod-attribute name name)
;;             (kubel--propertize-pod-attribute name ready)
;;             (kubel--propertize-status status)
;;             (kubel--propertize-pod-attribute name restarts)
;;             (kubel--propertize-pod-attribute name age))))

(defun kubel--propertize-pod-attribute (name attribute)
  "Return the pod attribute in proper font color based on active filter.

NAME is the pod name.
ATTRIBUTE is the attribute to propertize."
  (if (or (equal kubel-pod-filter "") (string-match-p kubel-pod-filter name))
      attribute
    (propertize attribute 'font-lock-face '(:foreground "darkgrey"))))

(defun kubel--propertize-status (status)
  "Return the status in proper font color.

STATUS is the pod status string."
  (let ((pair (cdr (assoc status kubel--status-colors))))
    (if pair
        (propertize status 'font-lock-face `(:foreground ,pair))
      status)))

;; (defun kubel--list-entries ()
;;   "Create the entries for the service list."
;;   (let ((temp (list)))
;;     (with-temp-buffer
;;       (insert (shell-command-to-string (concat (kubel--get-command-prefix) " get " kubel-resource " --no-headers=true")))
;;       (goto-char (point-min))
;;       (while (re-search-forward "^\\([a-z0-9\-]+\\) +\\([0-9]+/[0-9]+\\) +\\(\\w+\\) +\\([0-9]+\\) +\\([0-9a-z]+\\)$" (point-max) t)
;;         (setq temp (append temp (list (list (match-string 1) (kubel--extract-pod-line)))))))
;;     temp))

(defun kubel--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer name))

(defun kubel--exec (buffer-name async args)
  "Utility function to run commands in the proper context and namespace.

\"BUFFER-NAME\" is the buffer-name. Default to *kubel-command*.
ASYNC is a bool. If true will run async.
ARGS is a ist of arguments."
  (when (equal buffer-name "")
    (setq buffer-name "*kubel-command*"))
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (if async
      (apply #'start-process buffer-name buffer-name "kubectl" (append (kubel--get-context-namespace) args))
    (apply #'call-process "kubectl" nil buffer-name nil (append (kubel--get-context-namespace) args)))
  (pop-to-buffer buffer-name))

(defun kubel--get-resource-under-cursor ()
  "Utility function to get the name of the pod under the cursor."
  (aref (tabulated-list-get-entry) 0))

(defun kubel--get-context-namespace ()
  "Utility function to return the proper context and namespace arguments."
  (append
   (unless (equal kubel-context "")
     (list "--context" kubel-context))
   (unless (equal kubel-namespace "")
     (list "-n" kubel-namespace))))

(defun kubel--get-command-prefix ()
  "Utility function to prefix the kubectl command with proper context and namespace."
  (mapconcat 'identity (append '("kubectl") (kubel--get-context-namespace)) " "))

(defun kubel--get-containers (pod-name)
  "List the containers in a pod.

POD-NAME is the name of the pod."
  (split-string
   (shell-command-to-string
    (format "%s get pod %s -o jsonpath='{.spec.containers[*].name}'" (kubel--get-command-prefix) pod-name)) " "))

(defun kubel--select-resource (name)
  "Prompt user to select an instance out of a list of resources.

NAME is the string name of the resource."
  (let ((cmd (format "%s get %s -o=jsonpath='{.items[*].metadata.name}'"
                     (kubel--get-command-prefix) name)))
    (completing-read (concat (s-upper-camel-case name) ": ")
                     (split-string (shell-command-to-string cmd) " "))))

(defun kubel--describe-resource (name &optional yaml)
  "Describe a specific resource.

NAME is the string name of the resource to decribe.
YAML is boolean to show resource as yaml"
  (let* ((resource (kubel--select-resource name))
         (buffer-name (format "*kubel - %s - %s*" name resource)))
    (if yaml
        (kubel--exec buffer-name nil (list "get" name "-o" "yaml" resource))
      (kubel--exec buffer-name nil (list "describe" name resource)))
    (when yaml
      (yaml-mode)
      (kubel-yaml-editing-mode))
    (beginning-of-buffer)))

(defun kubel--show-rollout-revision (type)
  "Show a specific revision of a certain resource.

TYPE is the resource type to prompt you to select a specific one."
  (let* ((name (kubel--select-resource type))
         (typename (format "%s/%s" type name))
         (revision (car (split-string (kubel--select-rollout typename))))
         (buffer-name (format "*kubel - rollout - %s - %s*" typename revision)))
    (kubel--exec buffer-name nil
                 (list "rollout" "history" typename (format "--revision=%s" revision)))
    (beginning-of-buffer)))

(defun kubel--list-rollout (typename)
  "Return a list of revisions with format '%number   %cause'.

TYPENAME is the resource type/name."
  (let ((cmd (format "%s rollout history %s" (kubel--get-command-prefix) typename)))
    (nthcdr 2 (split-string (shell-command-to-string cmd) "\n"))))

(defun kubel--select-rollout (typename)
  "Select a rollout version.

TYPENAME is the resource type/name."
  (let ((prompt (format "Select a rollout of %s: " typename))
        (rollouts (kubel--list-rollout typename)))
    (completing-read prompt rollouts)))

;; interactive
(define-minor-mode kubel-yaml-editing-mode
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'kubel-apply)
            map))

(defun kubel-apply ()
  "Save the current buffer to a temp file and try to kubectl apply it."
  (interactive)
  (let ((filename (format "/tmp/kubel/%s-%s.yaml"
                          (replace-regexp-in-string "\*\\| " "" (buffer-name))
                          (floor (float-time)))))
    (unless  (file-exists-p "/tmp/kubel")
      (make-directory "/tmp/kubel"))
    (write-region (point-min) (point-max) filename)
    (kubel--exec (format "*kubectl - apply - %s*" filename) nil (list "apply" "-f" filename))
    (message "Applied %s" filename)))

(defun kubel-get-resource-details ()
  "Get the details of the resource under the cursor."
  (interactive)
  (let* ((pod (kubel--get-resource-under-cursor))
         (buffer-name (format "*kubel - %s - %s*" kubel-resource pod)))
    (kubel--exec buffer-name nil (list "describe" kubel-resource (kubel--get-resource-under-cursor)))
    (beginning-of-buffer)))


(defun kubel--default-tail-arg (args)
  "Ugly function to make sure that there is at least the default tail.

ARGS is the arg list from transient."
  (if (car (remove nil (mapcar (lambda (x)
                                 (string-prefix-p "--tail=" x)) args)))
      args
    (append args (list (concat "--tail=" kubel-log-tail-n)))))

(defun kubel-get-pod-logs (&optional args)
  "Get the last N logs of the pod under the cursor.

ARGS is the arguments list from transient."
  (interactive
   (list (transient-args 'kubel-log-popup)))
  (let* ((pod (kubel--get-resource-under-cursor))
         (containers (kubel--get-containers pod))
         (container (if (equal (length containers) 1)
                        (car containers)
                      (completing-read "Select container: " containers)))
         (buffer-name (format "*kubel - logs - %s - %s*" pod container))
         (async nil))
    (when (member "-f" args)
      (setq async t))
    (kubel--exec buffer-name async
                 (append '("logs") (kubel--default-tail-arg args) (list pod container)))))

(defun kubel-copy-pod-name ()
  "Copy the name of the pod under the cursor."
  (interactive)
  (kill-new (kubel--get-resource-under-cursor))
  (message "Pod name copied to kill-ring"))

(defun kubel-copy-log-command ()
  "Copy the streaming log command of the pod under the cursor."
  (interactive)
  (kill-new (concat (kubel--get-command-prefix)
                    " logs -f --tail=" kubel-log-tail-n " "
                    (kubel--get-resource-under-cursor)))
  (message "Log command copied to kill-ring"))

(defun kubel-copy-command-prefix ()
  "Copy the kubectl command prefix."
  (interactive)
  (kill-new (kubel--get-command-prefix))
  (message "Command prefix copied to kill-ring"))

(defun kubel-set-namespace ()
  "Set the namespace."
  (interactive)
  (let ((namespace (completing-read "Namespace: " kubel-namespace-history
                                    nil nil nil nil "(empty)")))
    (when (get-buffer (kubel--buffer-name))
      (kill-buffer (kubel--buffer-name)))
    (setq kubel-namespace
          (if (equal "(empty)" namespace) "" namespace))
    (unless (member namespace kubel-namespace-history)
      (push namespace kubel-namespace-history))
    (kubel)))

(defun kubel-set-context ()
  "Set the context."
  (interactive)
  (when (get-buffer (kubel--buffer-name)) ;; kill buffer for previous context if possible
    (kill-buffer (kubel--buffer-name)))
  (setq kubel-context
        (completing-read
         "Select context: "
         (split-string (shell-command-to-string "kubectl config view -o jsonpath='{.contexts[*].name}'") " ")))
  (kubel))

(defun kubel-set-resource ()
  "Set the resource."
  (interactive)
  (let  ((resource-list (if (kubel-kubernetes-compatible-p '(1 13 3))
			    (split-string (shell-command-to-string "kubectl api-resources -o name --no-headers=true") "\n")
			 kubel-kubernetes-resources-list
			  )))
    (setq kubel-resource
	  (completing-read
	   "Select resource: "
	   resource-list
	   ))
    )
  (kubel))


(defun kubel-port-forward-pod (p)
  "Port forward a pod to your local machine.

P is the port as integer."
  (interactive "nPort: ")
  (let* ((port (format "%s" p))
         (pod (kubel--get-resource-under-cursor))
         (buffer-name (format "*kubel - port-forward - %s:%s*" pod port)))
    ;; TODO error message if resource is not pod
    (kubel--exec buffer-name t (list "port-forward" pod (format "%s:%s" port port)))))

;; Obsoleted by kubel-describe-resource
;; TODO test & remove
;; (defun kubel-describe-ingress (&optional arg)
;;   "Show the ingress details.

;; ARG is the optional param to see yaml."
;;   (interactive "P")
;;   (if (or arg (transient-args 'kubel-describe-popup))
;;       (kubel--describe-resource "ingress" t)
;;     (kubel--describe-resource "ingress")))


;; (defun kubel-describe-service (&optional arg)
;;   "Descibe a service.

;; ARG is the optional param to see yaml."
;;   (interactive "P")
;;   (if (or arg (transient-args 'kubel-describe-popup))
;;       (kubel--describe-resource "service" t)
;;     (kubel--describe-resource "service")))

;; (defun kubel-describe-configmaps (&optional arg)
;;   "Describe a configmap.

;; ARG is the optional param to see yaml."
;;   (interactive "P")
;;   (if (or arg (transient-args 'kubel-describe-popup))
;;       (kubel--describe-resource "configmap" t)
;;     (kubel--describe-resource "configmap")))

;; (defun kubel-describe-deployment (&optional arg)
;;   "Describe a deployment.

;; ARG is the optional param to see yaml."
;;   (interactive "P")
;;   (if (or arg (transient-args 'kubel-describe-popup))
;;       (kubel--describe-resource "deployment" t)
;;     (kubel--describe-resource "deployment")))

;; (defun kubel-describe-job (&optional arg)
;;   "Describe a job.

;; ARG is the optional param to see yaml."
;;   (interactive "P")
;;   (if (or arg (transient-args 'kubel-describe-popup))
;;       (kubel--describe-resource "job" t)
;;     (kubel--describe-resource "job")))

;; deprecated. will remove soon
;; (defun kubel-exec-pod ()
;;   "Kubectl exec into the pod under the cursor."
;;   (interactive)
;;   (let* ((pod (kubel--get-pod-under-cursor))
;;          (containers (kubel--get-containers pod))
;;          (container (if (equal (length containers) 1)
;;                         (car containers)
;;                       (completing-read "Select container: " containers))))
;;     (eshell)
;;     (insert (format "%s exec -it %s -c %s /bin/sh" (kubel--get-command-prefix) pod container))))

(defun kubel-exec-pod ()
  "Setup a TRAMP to exec into the pod under the cursor."
  (interactive)
  (setq tramp-methods (delete (assoc "kubectl" tramp-methods) tramp-methods)) ;; cleanup previous tramp method
  ;; TODO error message if resource is not pod
  (add-to-list 'tramp-methods
               `("kubectl"
                 (tramp-login-program      "kubectl")
                 (tramp-login-args         (,(kubel--get-context-namespace) ("exec" "-it") ("-u" "%u") ("%h") ("sh")))
                 (tramp-remote-shell       "sh")
                 (tramp-remote-shell-args  ("-i" "-c")))) ;; add the current context/namespace to tramp methods
  (find-file (format "/kubectl:%s:/" (kubel--get-resource-under-cursor))))

(defun kubel-delete-resource ()
  "Kubectl delete resource under cursor."
  (interactive)
  (let* ((pod (kubel--get-resource-under-cursor))
         (buffer-name (format "*kubel - delete %s -%s" kube-resource pod))
         (args (list "delete" kube-resource pod)))
    (when (transient-args 'kubel-delete-popup)
      (setq args (append args (list "--force" "--grace-period=0"))))
    (kubel--exec buffer-name t args)))

(defun kubel-jab-deployment ()
  "Make a trivial patch to force a new deployment.

See https://github.com/kubernetes/kubernetes/issues/27081"
  (interactive)
  (let* ((deployment (kubel--select-resource "deployment"))
         (buffer-name (format "*kubel - bouncing - %s*" deployment)))
    (kubel--exec buffer-name nil (list "patch" "deployment" deployment "-p"
				       (format "{\"spec\":{\"template\":{\"metadata\":{\"labels\":{\"date\":\"%s\"}}}}}"
					       (round (time-to-seconds)))))))

(defun kubel-set-filter (filter)
  "Set the pod filter.

FILTER is the filter string."
  (interactive "MFilter: ")
  (setq kubel-pod-filter filter)
  (kubel-mode))

(defun kubel-rollout-history-deployment ()
  "See rollout history of a deployment."
  (interactive)
  (kubel--show-rollout-revision "deployment"))

(defun kubel-rollout-history-service ()
  "See rollout history of a service."
  (interactive)
  (kubel--show-rollout-revision "service"))

(defun kubel-rollout-history-job ()
  "See rollout history of a job."
  (interactive)
  (kubel--show-rollout-revision "job"))

(defun kubel-rollout-history-ingress ()
  "See a rollout history of an ingress."
  (interactive)
  (kubel--show-rollout-revision "ingress"))

(defun kubel-rollout-history-configmap ()
  "See a rollout history of a configmap."
  (interactive)
  (kubel--show-rollout-revision "configmap"))

;; popups

(define-transient-command kubel-log-popup ()
  "Kubel Log Menu"
  ["Arguments"
   ("-f" "Follow" "-f")
   ("-p" "Previous" "-p")
   ("-n" "Tail" "--tail=")]
  ["Actions"
   ("l" "Tail pod logs" kubel-get-pod-logs)])

(define-transient-command kubel-copy-popup ()
  "Kubel Copy Menu"
  ["Actions"
   ("c" "Copy pod name" kubel-copy-pod-name)
   ("l" "Copy pod log command" kubel-copy-log-command)
   ("p" "Copy command prefix" kubel-copy-command-prefix)])

(define-transient-command kubel-delete-popup ()
  "Kubel Delete menu"
  ["Arguments"
   ("-f" "Force" "--force --grace-period=0")]
  ["Actions"
   ("k" "Delete pod" kubel-delete-pod)])

(define-transient-command kubel-describe-popup ()
  "Kubel Describe Menu"
  ["Arguments"
   ("-y" "Yaml" "-o yaml")]
  ["Actions"
   ("d" "Deployment" kubel-describe-deployment)
   ("s" "Service" kubel-describe-service)
   ("j" "Job" kubel-describe-job)
   ("i" "Ingress" kubel-describe-ingress)
   ("c" "Configmap" kubel-describe-configmaps)])

(define-transient-command kubel-rollout-popup ()
  "Kubel Rollout Menu"
  ["Actions"
   ("d" "Deployment" kubel-rollout-history-deployment)
   ("s" "Service" kubel-rollout-history-service)
   ("j" "Job" kubel-rollout-history-job)
   ("i" "Ingress" kubel-rollout-history-ingress)
   ("c" "Configmap" kubel-rollout-history-configmap)])

(define-transient-command kubel-help-popup ()
  "Kubel Menu"
  ["Actions"
   ("ENTER" "Pod details" kubel-get-resource-details)
   ("C" "Set context" kubel-set-context)
   ("n" "Set namespace" kubel-set-namespace)
   ("g" "Refresh" kubel-mode)
   ("p" "Port forward" kubel-port-forward-pod)
   ("l" "Logs" kubel-log-popup)
   ("c" "Copy" kubel-copy-popup)
   ("d" "Describe" kubel-describe-popup)
   ("e" "Exec" kubel-exec-pod)
   ("k" "Delete" kubel-delete-popup)
   ("j" "Jab" kubel-jab-deployment)
   ("f" "Filter" kubel-set-filter)
   ("r" "Rollout" kubel-rollout-popup)
   ("R" "Set resource" kubel-set-resource)])

;; mode map
(defvar kubel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kubel-get-resource-details)
    (define-key map (kbd "C") 'kubel-set-context)
    (define-key map (kbd "n") 'kubel-set-namespace)
    (define-key map (kbd "g") 'kubel-mode)
    (define-key map (kbd "p") 'kubel-port-forward-pod)
    (define-key map (kbd "l") 'kubel-log-popup)
    (define-key map (kbd "c") 'kubel-copy-popup)
    (define-key map (kbd "h") 'kubel-help-popup)
    (define-key map (kbd "d") 'kubel-describe-popup)
    (define-key map (kbd "e") 'kubel-exec-pod)
    (define-key map (kbd "k") 'kubel-delete-popup)
    (define-key map (kbd "j") 'kubel-jab-deployment)
    (define-key map (kbd "f") 'kubel-set-filter)
    (define-key map (kbd "r") 'kubel-rollout-popup)
    (define-key map (kbd "R") 'kubel-set-resource)
    map)
  "Keymap for `kubel-mode'.")

;;;###autoload
(defun kubel ()
  "Invoke the kubel buffer."
  (interactive)
  (kubel--pop-to-buffer (kubel--buffer-name))
  (kubel-mode)
  (message (concat "Namespace: " kubel-namespace)))

(define-derived-mode kubel-mode tabulated-list-mode "Kubel"
  "Special mode for kubel buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Kubel")
  (setq major-mode 'kubel-mode)
  (use-local-map kubel-mode-map)
  (setq entries (kubel--populate-list))
  (setq tabulated-list-format (car entries))
  (setq tabulated-list-entries (cadr entries))   ; TODO handle "No resource found"
  (setq tabulated-list-sort-key kubel--list-sort-key)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (run-mode-hooks 'kubel-mode-hook))

(provide 'kubel)
;;; kubel.el ends here
