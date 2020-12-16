;;; kubel.el --- Control Kubernetes with limited permissions -*- lexical-binding: t; -*-

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
;; Package-Requires: ((transient "0.1.0") (emacs "25.3") (dash "2.12.0") (s "1.2.0") (yaml-mode "0.0.14"))

;;; Commentary:

;; Emacs extension for controlling Kubernetes with limited permissions.

;;; Usage:

;; To list the pods in your current context and namespace, call
;;
;; M-x kubel
;;
;; To set the path to the kubectl config file, call:
;; M-x kubel-set-kubectl-config-file
;;
;; or
;;
;; (kubel-set-kubectl-config-file <path to desired config file>)
;; ex: (kubel-set-kubectl-config-file "~/.kube/another-config")
;;
;; To set said namespace and context, respectively call
;;
;; M-x kubel-set-namespace
;; M-x kubel-set-context
;;
;; Note that namespace will autocomplete but not context,
;; this is because I interact with kubernetes through a user who
;; does not have permissions to list namespaces.
;;
;; To switch to showing a different resource, use the `R` command or
;;
;; M-x kubel-set-resource
;;
;; This will let you select a resource and re-display the kubel buffer.

;;; Shortcuts:

;; On the kubel screen, place your cursor on a resource
;;
;; enter => get resource details
;; h => help popup
;; C => set context
;; n => set namespace
;; R => set resource
;; F => set output format
;; g => refresh
;; f => set a substring filter for resource name
;; M-n => jump to the next highlighted resource
;; M-p => jump to previous highlighted resource
;; E => quick edit any resource
;; r => see the rollout history for resource
;; l => log popup
;; c => copy popup
;; k => delete popup
;; e => exec into pod
;; p => port forward pod
;; j => jab deployment to force rolling update

;;; Customize:

;; By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.

;;; Code:

(require 'transient)
(require 'dash)
(require 's)
(require 'yaml-mode)
(require 'tramp)
(require 'subr-x)

(defgroup kubel nil "Customisation group for kubel."
  :group 'extensions)

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
    ("True" . "green")
    ("Unknown" . "orange")
    ("Error" . "red")
    ("MemoryPressure" . "red")
    ("PIDPressure" . "red")
    ("DiskPressure" . "red")
    ("RevisionMissing" . "red")
    ("RevisionFailed" . "red")
    ("NetworkUnavailable" . "red")
    ("Completed" . "yellow")
    ("CrashLoopBackOff" . "red")
    ("Terminating" . "blue"))
  "Associative list of status to color.")

(defcustom kubel-output "yaml"
  "Format for output: json|yaml|wide|custom-columns=..."
  :type 'string
  :group 'kubel)

(defcustom kubel-log-tail-n 100
  "Default number of lines to tail."
  :type 'integer
  :group 'kubel)

(defcustom kubel-use-namespace-list 'auto
  "Control behavior for namespace completion.

auto - default, use `kubectl auth can-i list namespace` to determine if we can list namespaces
on - always assume we can list namespaces
off - always assume we cannot list namespaces"
  :type 'symbol
  :group 'kubel
  :options '('auto 'on 'off))

(defvar kubel-namespace "default"
  "Current namespace.")

(defvar kubel-resource "Pods"
  "Current resource.")

(defvar kubel-context
  (replace-regexp-in-string
   "\n" "" (shell-command-to-string "kubectl config current-context"))
  "Current context.  Tries to smart default.")

(defvar kubel-resource-filter ""
  "Substring filter for resource name.")

(defvar kubel--line-number nil
  "Store the current line number to jump back after a refresh.")

(defvar kubel-namespace-history '()
  "List of previously used namespaces.")

;; fallback list of resources if the version of kubectl doesn't support api-resources command
(defvar kubel-kubernetes-resources-list
  '("Pods"
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
    "hpa"
	"Images"
	"Ingresses"
	"ClusterRoles"
	"RoleBindings"
	"Roles"))

(defvar kubel--kubernetes-version-cached nil)

(defvar kubel--kubernetes-resources-list-cached nil)

(defvar kubel--can-get-namespace-cached nil)

(defvar kubel--namespace-list-cached nil)

(defun kubel--invalidate-context-caches ()
  "Invalidate the context caches."
  (setq kubel--kubernetes-resources-list-cached nil)
  (setq kubel--kubernetes-version-cached nil)
  (setq kubel--can-get-namespace-cached nil)
  (setq kubel--namespace-list-cached nil))

(defun kubel-kubernetes-version ()
  "Return a list with (major-version minor-version patch)."
  (let ((version-string (if (null kubel--kubernetes-version-cached)
                            (setq kubel--kubernetes-version-cached
                                  (shell-command-to-string "kubectl version"))
                          kubel--kubernetes-version-cached)))
    (string-match "GitVersion:\"v\\([0-9]*\\)\.\\([0-9]*\\)\.\\([0-9]*\\)[^0-9].*\"" version-string)
    (list
     (string-to-number (match-string 1 version-string))
     (string-to-number (match-string 2 version-string))
     (string-to-number (match-string 3 version-string)))))

(defun kubel-kubernetes-compatible-p (version)
  "Return TRUE if kubernetes version is greater than or equal to VERSION.
VERSION should be a list of (major-version minor-version patch)."
  (let*
      ((kubernetes-version (kubel-kubernetes-version))
       (kubernetes-major-version (nth 0 kubernetes-version))
       (kubernetes-minor-version (nth 1 kubernetes-version))
       (kubernetes-patch-version (nth 2 kubernetes-version)))
    (and
     (<= (nth 0 version) kubernetes-major-version)
     (or (<= (nth 1 version) kubernetes-minor-version) (< (nth 0 version) kubernetes-major-version))
     (or (<= (nth 2 version) kubernetes-patch-version) (< (nth 1 version) kubernetes-minor-version)))))

(defun kubel--populate-list ()
  "Return a list with a tabulated list format and \"tabulated-list-entries\"."
  (let*  ((body (shell-command-to-string (concat (kubel--get-command-prefix) " get " kubel-resource)))
	      (entrylist (kubel--parse-body body)))
    (when (string-prefix-p "No resources found" body)
	  (message "No resources found"))  ;; TODO exception here
    (list (kubel--get-list-format entrylist) (kubel--get-list-entries entrylist))))

(defun kubel--column-entry (entrylist)
  "Return a function of colnum to retrieve an entry in a given column for ENTRYLIST."
  (function
   (lambda (colnum)
     (list (kubel--column-header entrylist colnum) (+ 4 (kubel--column-width entrylist colnum)) t))))


(defun kubel--get-list-format (entrylist)
  "Get the list format.

ENTRYLIST is the output of the parsed body."
  (defun kubel--get-column-entry (colnum)
    (let ((kubel--get-entry (kubel--column-entry entrylist)))
	  (funcall kubel--get-entry colnum)))
  (cl-map 'vector #'kubel--get-column-entry (number-sequence 0 (- (kubel--ncols entrylist) 1))))

(defun kubel--get-list-entries (entrylist)
  "Get the entries.

ENTRYLIST is the output of the parsed body."
  (mapcar (lambda (x)
            (list (car x)
                  (vconcat [] (mapcar #'kubel--propertize-status x))))
          (cdr entrylist)))

(defun kubel--parse-body (body)
  "Parse the body of kubectl get resource call into a list.

BODY is the raw output of kubectl get resource."
  (let* ((lines (nbutlast (split-string body "\n")))
         (header (car lines))
         (cols (split-string header))
         (start-pos (mapcar (lambda (x) (string-match x header)) cols))
         (end-pos (delete 0 (append start-pos '("end"))))
         (position (-zip-with 'cons start-pos end-pos))
         (parse-line (lambda (line)
                       (mapcar (lambda (pos)
                                 (kubel--extract-value line (car pos) (cdr pos)))
                               position))))
    (mapcar parse-line lines)))

(defun kubel--extract-value (line min max)
  "Extract value from LINE between MIN and MAX.
If it's just white space, return -, else trim space.
If MAX is the end of the line, dynamically adjust."
  (let* ((maxx (if (equal max "end") (length line) max))
         (str (substring-no-properties line min maxx)))
    (if (string-match "^ +$" str)
        "-"
      (string-trim str))))

(defun kubel--ncols (entrylist)
  "Return the number of columns in ENTRYLIST."
  (length (car entrylist)))

(defun kubel--nrows (entrylist)
  "Return the nubmer of rows in ENTRYLIST."
  (length entrylist))

(defun kubel--column-header (entrylist colnum)
  "Return the header for a specific COLNUM in ENTRYLIST."
  (nth colnum (car entrylist)))

(defun kubel--column-width (entrylist colnum)
  "Return the width of a specific COLNUM in ENTRYLIST."
  (seq-max (mapcar (lambda (x) (length (nth colnum x) )) entrylist)))

(defun kubel--buffer-name ()
  "Return kubel buffer name."
  (format "*kubel (%s) [%s]: %s*" kubel-namespace kubel-context kubel-resource))

(defun kubel--propertize-status (status)
  "Return the status in proper font color.

STATUS is the pod status string."
  (let ((pair (cdr (assoc status kubel--status-colors)))
        (match (or (equal kubel-resource-filter "") (string-match-p kubel-resource-filter status))))
    (cond (pair (propertize status 'font-lock-face `(:foreground ,pair)))
          ((not match) (propertize status 'font-lock-face '(:foreground "darkgrey")))
          (t status))))

(defun kubel--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer-same-window name))

(defun kubel--exec (buffer-name async args &optional readonly)
  "Utility function to run commands in the proper context and namespace.

\"BUFFER-NAME\" is the buffer-name. Default to *kubel-command*.
ASYNC is a bool. If true will run async.
ARGS is a ist of arguments.
READONLY If true buffer will be in readonly mode(view-mode)."
  (when (equal buffer-name "")
    (setq buffer-name "*kubel-command*"))
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (if async
      (apply #'start-file-process buffer-name buffer-name "kubectl" (append (kubel--get-context-namespace) args))
    (apply #'process-file "kubectl" nil buffer-name nil (append (kubel--get-context-namespace) args)))
  (pop-to-buffer buffer-name)
  (if readonly
      (with-current-buffer buffer-name
        (view-mode))))

(defun kubel--get-resource-under-cursor ()
  "Utility function to get the name of the pod under the cursor."
  (aref (tabulated-list-get-entry) 0))

(defun kubel--get-context-namespace ()
  "Utility function to return the proper context and namespace arguments."
  (append
   (unless (equal kubel-context "")
     (list "--context" kubel-context))
   (unless (equal kubel-namespace "default")
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

(defun kubel--get-pod-labels ()
  "List labels of pods in a current namespace."
  (delete-dups
   (split-string
    (replace-regexp-in-string
     (regexp-quote ":") "="
     (replace-regexp-in-string
      "map\\[\\(.+?\\)\\]" "\\1"
      (shell-command-to-string
       (format "%s get pod -o jsonpath='{.items[*].metadata.labels}'" (kubel--get-command-prefix))))))))

(defun kubel--select-resource (name)
  "Prompt user to select an instance out of a list of resources.

NAME is the string name of the resource."
  (let ((cmd (format "%s get %s -o=jsonpath='{.items[*].metadata.name}'"
                     (kubel--get-command-prefix) name)))
    (completing-read (concat (s-upper-camel-case name) ": ")
                     (split-string (shell-command-to-string cmd) " "))))

(defun kubel--describe-resource (name &optional describe)
  "Describe a specific resource.

NAME is the string name of the resource to decribe.
DESCRIBE is boolean to describe instead of get resource details"
  (let* ((resource (kubel--select-resource name))
         (buffer-name (format "*kubel - %s - %s*" name resource)))
    (if describe
	    (kubel--exec buffer-name nil (list "describe" name resource))
      (kubel--exec buffer-name nil (list "get" name "-o" kubel-output resource)))
    (when (string-equal kubel-output "yaml")
      (yaml-mode)
      (kubel-yaml-editing-mode))
    (goto-char (point-min))))

(defun kubel--show-rollout-revision (type name)
  "Show a specific revision of a certain resource.

TYPE is the resource type.
NAME is the resource name."
  (let* ((typename (format "%s/%s" type name))
         (revision (car (split-string (kubel--select-rollout typename))))
         (buffer-name (format "*kubel - rollout - %s - %s*" typename revision)))
    (kubel--exec buffer-name nil
                 (list "rollout" "history" typename (format "--revision=%s" revision)))
    (goto-char (point-min))))

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

(defun kubel--is-pod-view ()
  "Return non-nil if this is the pod view."
  (equal (capitalize kubel-resource) "Pods"))

(defun kubel--is-deployment-view ()
  "Return non-nil if this is the pod view."
  (equal kubel-resource "Deployments"))

(defun kubel--save-line ()
  "Save the current line number if the view is unchanged."
  (if (equal (buffer-name (current-buffer))
             (kubel--buffer-name))
      (setq kubel--line-number (+ 1 (count-lines 1 (point))))
    (setq kubel--line-number nil)))

(defun kubel--jump-back-to-line ()
  "Jump back to the last cached line number."
  (when kubel--line-number
    (goto-line kubel--line-number)))

;; interactive
(define-minor-mode kubel-yaml-editing-mode
  "Kubel Yaml editing mode.
Use C-c C-c to kubectl apply the current yaml buffer."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'kubel-apply)
            map))

(defun kubel-apply ()
  "Save the current buffer to a temp file and try to kubectl apply it."
  (interactive)
  (setq dir-prefix (or
		            (when (tramp-tramp-file-p default-directory)
		              (with-parsed-tramp-file-name default-directory nil
			            (format "/%s%s:%s@%s:" (or hop "") method user host)))
		            ""))

  (let* ((filename-without-tramp-prefix (format "/tmp/kubel/%s-%s.yaml"
						                        (replace-regexp-in-string "\*\\| " "" (buffer-name))
						                        (floor (float-time))))
	     (filename (format "%s%s" dir-prefix filename-without-tramp-prefix)))
    (when (y-or-n-p "Apply the changes? ")
      (unless  (file-exists-p (format "%s/tmp/kubel" dir-prefix))
	    (make-directory (format "%s/tmp/kubel" dir-prefix) t))
      (write-region (point-min) (point-max) filename)
      (kubel--exec (format "*kubectl - apply - %s*" filename) nil (list "apply" "-f" filename-without-tramp-prefix))
      (message "Applied %s" filename))))

(defun kubel-get-resource-details (&optional describe)
  "Get the details of the resource under the cursor.

 DESCRIBE is the optional param to describe instead of get."
  (interactive "P")
  (let* ((resource (kubel--get-resource-under-cursor))
         (buffer-name (format "*kubel - %s - %s*" kubel-resource resource)))
    (if describe
        (kubel--exec buffer-name nil (list "describe" kubel-resource (kubel--get-resource-under-cursor)))
      (kubel--exec buffer-name nil (list "get" kubel-resource (kubel--get-resource-under-cursor) "-o" kubel-output)))
    (when (or (string-equal kubel-output "yaml") (transient-args 'kubel-describe-popup))
      (yaml-mode)
      (kubel-yaml-editing-mode))
    (goto-char (point-min))))


(defun kubel--default-tail-arg (args)
  "Ugly function to make sure that there is at least the default tail.

ARGS is the arg list from transient."
  (if (car (remove nil (mapcar (lambda (x)
                                 (string-prefix-p "--tail=" x)) args)))
      args
    (append args (list (concat "--tail=" (format "%s" kubel-log-tail-n))))))

(defun kubel-get-pod-logs (&optional args)
  "Get the last N logs of the pod under the cursor.

ARGS is the arguments list from transient."
  (interactive
   (list (transient-args 'kubel-log-popup)))
  (let* ((pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (containers (kubel--get-containers pod))
         (container (if (equal (length containers) 1)
                        (car containers)
                      (completing-read "Select container: " containers)))
         (buffer-name (format "*kubel - logs - %s - %s*" pod container))
         (async nil))
    (when (member "-f" args)
      (setq async t))
    (kubel--exec buffer-name async
                 (append '("logs") (kubel--default-tail-arg args) (list pod container)) t)))

(defun kubel-get-logs-by-labels (&optional args)
  "Get the last N logs of the pods by labels.
ARGS is the arguments list from transient."
  (interactive
   (list (transient-args 'kubel-log-popup)))
  (let* ((labels (kubel--get-pod-labels))
         (label (completing-read "Select container: " labels))
         (buffer-name (format "*kubel - logs - %s*" label))
         (async nil))
    (when (member "-f" args)
      (setq async t))
    (kubel--exec buffer-name async
                 (append '("logs") (kubel--default-tail-arg args) '("-l") (list label)) t)))

(defun kubel-copy-resource-name ()
  "Copy the name of the pod under the cursor."
  (interactive)
  (kill-new (kubel--get-resource-under-cursor))
  (message "Resource name copied to kill-ring"))

(defun kubel-copy-log-command ()
  "Copy the streaming log command of the pod under the cursor."
  (interactive)
  (kill-new
   (format "%s logs -f --tail=%s %s"
           (kubel--get-command-prefix)
           kubel-log-tail-n
           (if (kubel--is-pod-view)
               (kubel--get-resource-under-cursor)
             (kubel--select-resource "Pods"))))
  (message "Log command copied to kill-ring"))

(defun kubel-copy-command-prefix ()
  "Copy the kubectl command prefix."
  (interactive)
  (kill-new (kubel--get-command-prefix))
  (message "Command prefix copied to kill-ring"))

(defun kubel-set-kubectl-config-file (configfile)
  "Set the path to the kubectl CONFIGFILE."
  (interactive "f")
  (let ((configfile (or configfile "~/.kube/config")))
    (if (file-exists-p (expand-file-name configfile))
	    (setenv "KUBECONFIG" (expand-file-name configfile))
      (error "Kubectl config file '%s' does not exist!" configfile))))

(defun kubel--can-get-namespace ()
  "Determine if permissions allow for `kubectl get namespace` in current context."
  (cond ((eq kubel-use-namespace-list 'on) t)
        ((eq kubel-use-namespace-list 'auto)
         (progn
           (unless kubel--can-get-namespace-cached
             (setq kubel--can-get-namespace-cached
                   (equal "yes\n"
                          (shell-command-to-string
                           (format "kubectl --context %s auth can-i list namespaces" kubel-context))))))
         kubel--can-get-namespace-cached)))

(defun kubel--get-namespace ()
  "Get namespaces for current context, try to recover from cache first."
  (unless kubel--namespace-list-cached
    (setq kubel--namespace-list-cached
          (split-string (shell-command-to-string
                         (format "kubectl --context %s get namespace -o jsonpath='{.items[*].metadata.name}'" kubel-context)) " ")))
  kubel--namespace-list-cached)

(defun kubel--list-namespace ()
  "List namespace, either from history, or dynamically if possible."
  (if (kubel--can-get-namespace)
      (kubel--get-namespace)
    kubel-namespace-history))

(defun kubel--add-namespace-to-history (namespace)
  "Add NAMESPACE to history if it isn't there already."
  (unless (member namespace kubel-namespace-history)
    (push namespace kubel-namespace-history)))

(defun kubel-set-namespace ()
  "Set the namespace."
  (interactive)
  (let* ((namespace (completing-read "Namespace: " (kubel--list-namespace)
                                     nil nil nil nil "default"))
	     (kubel--buffer (get-buffer (kubel--buffer-name)))
	     (last-default-directory (when kubel--buffer
				                   (with-current-buffer kubel--buffer default-directory))))
    (when kubel--buffer (kill-buffer kubel--buffer))
    (setq kubel-namespace namespace)
    (kubel--add-namespace-to-history namespace)
    (kubel last-default-directory)))

(defun kubel-set-context ()
  "Set the context."
  (interactive)
  (let* ((kubel--buffer (get-buffer (kubel--buffer-name)))
	     (last-default-directory (when kubel--buffer (with-current-buffer kubel--buffer default-directory))))
    (when kubel--buffer (kill-buffer kubel--buffer));; kill buffer for previous context if possible
    (setq kubel-context
          (completing-read
           "Select context: "
           (split-string (shell-command-to-string "kubectl config view -o jsonpath='{.contexts[*].name}'") " ")))
    (kubel--invalidate-context-caches)
    (setq kubel-namespace "default")
    (kubel last-default-directory)))

(defun kubel--fetch-api-resource-list ()
  "Fetch the API resource list."
  (split-string (shell-command-to-string "kubectl api-resources -o name --no-headers=true") "\n"))

(defun kubel-set-resource (&optional refresh)
  "Set the resource.
If called with a prefix argument REFRESH, refreshes
the context caches, including the cached resource list."
  (interactive "P")
  (when refresh (kubel--invalidate-context-caches))
  (let* ((current-buffer-name (kubel--buffer-name))
         (resource-list (if (kubel-kubernetes-compatible-p '(1 13 3))
	                        (if (null kubel--kubernetes-resources-list-cached)
				                (setq kubel--kubernetes-resources-list-cached
                                      (kubel--fetch-api-resource-list))
                              kubel--kubernetes-resources-list-cached)
	                      kubel-kubernetes-resources-list))
	     (kubel--buffer (get-buffer current-buffer-name))
	     (last-default-directory (when kubel--buffer (with-current-buffer kubel--buffer default-directory))))
    (setq kubel-resource
	      (completing-read "Select resource: " resource-list))
    (when kubel--buffer (kill-buffer kubel--buffer)) ;; kill buffer for previous context if possible
    (kubel last-default-directory)))

(defun kubel-set-output-format ()
  "Set output format of kubectl."
  (interactive)
  (setq kubel-output
	    (completing-read
	     "Set output format: "
	     '("yaml" "json" "wide" "custom-columns=")))
  (kubel))

(defun kubel-port-forward-pod (p)
  "Port forward a pod to your local machine.

P can be a single number or a localhost:container port pair."
  (interactive "sPort: ")
  (let* ((port (if (string-match-p ":" p) p (format "%s:%s" p p)))
         (pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (buffer-name (format "*kubel - port-forward - %s:%s*" pod port)))
    (kubel--exec buffer-name t (list "port-forward" pod port))))

(defun kubel-setup-tramp ()
  "Setup a kubectl TRAMP."
  (setq tramp-methods (delete (assoc "kubectl" tramp-methods) tramp-methods)) ;; cleanup previous tramp method
  ;; TODO error message if resource is not pod
  (add-to-list 'tramp-methods
               `("kubectl"
                 (tramp-login-program      "kubectl")
                 (tramp-login-args         (,(kubel--get-context-namespace) ("exec" "-it") ("-u" "%u") ("%h") ("sh")))
                 (tramp-remote-shell       "sh")
                 (tramp-remote-shell-args  ("-i" "-c"))))) ;; add the current context/namespace to tramp methods

(defun kubel-exec-pod ()
  "Exec into the pod under the cursor -> `find-file."
  (interactive)
  (kubel-setup-tramp)
  (setq dir-prefix (or
		            (when (tramp-tramp-file-p default-directory)
		              (with-parsed-tramp-file-name default-directory nil
			            (format "%s%s:%s@%s|" (or hop "") method user host)))
		            ""))
  (find-file (format "/%skubectl:%s:/" dir-prefix (if (kubel--is-pod-view)
						                              (kubel--get-resource-under-cursor)
						                            (kubel--select-resource "Pods")))))

(defun kubel-exec-shell-pod ()
  "Exec into the pod under the cursor -> shell."
  (interactive)
  (kubel-setup-tramp)
  (let* ((dir-prefix (or
                    (when (tramp-tramp-file-p default-directory)
                      (with-parsed-tramp-file-name default-directory nil
                        (format "%s%s:%s@%s|" (or hop "") method user host))) ""))
         (pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (default-directory (format "/%skubectl:%s:/" dir-prefix pod)))
    (shell (format "*kubel - shell - %s*" pod))))

(defvar eshell-buffer-name)
(defun kubel-exec-eshell-pod ()
  "Exec into the pod under the cursor -> eshell."
  (interactive)
  (kubel-setup-tramp)
  (let* ((dir-prefix (or
                    (when (tramp-tramp-file-p default-directory)
                      (with-parsed-tramp-file-name default-directory nil
                        (format "%s%s:%s@%s|" (or hop "") method user host))) ""))
         (pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (default-directory (format "/%skubectl:%s:/" dir-prefix pod))
         (eshell-buffer-name (format "*kubel - eshell - %s*" pod)))
    (eshell)))

(defun kubel-delete-resource ()
  "Kubectl delete resource under cursor."
  (interactive)
  (let* ((pod (kubel--get-resource-under-cursor))
         (buffer-name (format "*kubel - delete %s -%s" kubel-resource pod))
         (args (list "delete" kubel-resource pod)))
    (when (transient-args 'kubel-delete-popup)
      (setq args (append args (list "--force" "--grace-period=0"))))
    (kubel--exec buffer-name t args)))

(defun kubel-jab-deployment ()
  "Make a trivial patch to force a new deployment.

See https://github.com/kubernetes/kubernetes/issues/27081"
  (interactive)
  (let* ((deployment
          (if (kubel--is-deployment-view)
              (kubel--get-resource-under-cursor)
            (kubel--select-resource "Deployments")))
         (buffer-name (format "*kubel - bouncing - %s*" deployment)))
    (kubel--exec buffer-name nil (list "patch" "deployment" deployment "-p"
				                       (format "{\"spec\":{\"template\":{\"metadata\":{\"labels\":{\"date\":\"%s\"}}}}}"
					                           (round (time-to-seconds)))))))

(defun kubel-set-filter (filter)
  "Set the pod filter.

FILTER is the filter string."
  (interactive "MFilter: ")
  (setq kubel-resource-filter filter)
  (kubel))

(defun kubel--jump-to-highlight (init search reset)
  "Base function to jump to highlight.

INIT is to be called before searching.
SEARCH is to apply the search and can be repeated safely.
RESET is to be called if the search is nil after the first attempt."
  (unless (equal kubel-resource-filter "")
    (funcall init)
    (unless (funcall search)
      (funcall reset)
      (funcall search))
    (beginning-of-line)))

(defun kubel-jump-to-next-highlight ()
  "Jump to the next hightlighted resrouce."
  (interactive)
  (kubel--jump-to-highlight
   #'end-of-line
   (lambda () (re-search-forward kubel-resource-filter (point-max) t))
   #'beginning-of-buffer))

(defun kubel-jump-to-previous-highlight ()
  "Jump to the previou highlighted resrouce."
  (interactive)
  (kubel--jump-to-highlight
   #'beginning-of-line
   (lambda () (re-search-backward kubel-resource-filter (point-min) t))
   #'end-of-buffer))

(defun kubel-rollout-history ()
  "See rollout history for resource under cursor."
  (interactive)
  (kubel--show-rollout-revision kubel-resource (kubel--get-resource-under-cursor)))

(defun kubel-changelog ()
  "Opens up the changelog."
  (interactive)
  (browse-url "https://github.com/abrochard/kubel/blob/master/CHANGELOG.md"))

(defun kubel-deprecated-warning ()
  "Show a warning to the user to inform of new workflow."
  (interactive)
  (message "This command has been deprecated, use the R key to select resource instead.\nYou can also checkout the changelog with `M-x kubel-changelog`"))

(defun kubel-quick-edit ()
  "Quickly edit any resource."
  (interactive)
  (kubel--describe-resource
   (completing-read "Select resource: " kubel-kubernetes-resources-list)))

;; popups

(define-transient-command kubel-exec-popup ()
  "Kubel Exec Menu"
  ["Actions"
   ("f" "Find files" kubel-exec-pod)
   ("e" "Eshell" kubel-exec-eshell-pod)
   ("s" "Shell" kubel-exec-shell-pod)])

(define-transient-command kubel-log-popup ()
  "Kubel Log Menu"
  ["Arguments"
   ("-f" "Follow" "-f")
   ("-p" "Previous" "-p")
   ("-n" "Tail" "--tail=")]
  ["Actions"
   ("l" "Tail pod logs" kubel-get-pod-logs)
   ("L" "Tail by labels" kubel-get-logs-by-labels)])

(define-transient-command kubel-copy-popup ()
  "Kubel Copy Menu"
  ["Actions"
   ("c" "Copy pod name" kubel-copy-resource-name)
   ("l" "Copy pod log command" kubel-copy-log-command)
   ("p" "Copy command prefix" kubel-copy-command-prefix)])

(define-transient-command kubel-delete-popup ()
  "Kubel Delete menu"
  ["Arguments"
   ("-f" "Force" "--force --grace-period=0")]
  ["Actions"
   ("k" "Delete resource" kubel-delete-resource)])

(define-transient-command kubel-describe-popup ()
  "Kubel Describe Menu"
  ["Arguments"
   ("-y" "Yaml" "-o yaml")]
  ["Actions"
   ("RET" "Describe" kubel-get-resource-details)])

(define-transient-command kubel-help-popup ()
  "Kubel Menu"
  ["Actions"
   ;; global
   ("RET" "Resource details" kubel-describe-popup)
   ("K" "Set kubectl config file" kubel-set-kubectl-config-file)
   ("C" "Set context" kubel-set-context)
   ("n" "Set namespace" kubel-set-namespace)
   ("g" "Refresh" kubel)
   ("F" "Set output format" kubel-set-output-format)
   ("R" "Set resource" kubel-set-resource)
   ("k" "Delete" kubel-delete-popup)
   ("f" "Filter" kubel-set-filter)
   ("M-n" "Next highlight" kubel-jump-to-next-highlight)
   ("M-p" "Previous highlight" kubel-jump-to-previous-highlight)
   ("r" "Rollout" kubel-rollout-history)
   ("E" "Quick edit" kubel-quick-edit)
   ;; based on current view
   ("p" "Port forward" kubel-port-forward-pod)
   ("l" "Logs" kubel-log-popup)
   ("c" "Copy" kubel-copy-popup)
   ("e" "Exec" kubel-exec-popup)
   ("j" "Jab" kubel-jab-deployment)])

;; mode map
(defvar kubel-mode-map
  (let ((map (make-sparse-keymap)))
    ;; global
    (define-key map (kbd "RET") 'kubel-get-resource-details)
    (define-key map (kbd "K") 'kubel-set-kubectl-config-file)
    (define-key map (kbd "C") 'kubel-set-context)
    (define-key map (kbd "n") 'kubel-set-namespace)
    (define-key map (kbd "g") 'kubel)
    (define-key map (kbd "h") 'kubel-help-popup)
    (define-key map (kbd "F") 'kubel-set-output-format)
    (define-key map (kbd "R") 'kubel-set-resource)
    (define-key map (kbd "k") 'kubel-delete-popup)
    (define-key map (kbd "f") 'kubel-set-filter)
    (define-key map (kbd "r") 'kubel-rollout-history)
    (define-key map (kbd "E") 'kubel-quick-edit)
    (define-key map (kbd "M-n") 'kubel-jump-to-next-highlight)
    (define-key map (kbd "M-p") 'kubel-jump-to-previous-highlight)
    ;; based on view
    (define-key map (kbd "p") 'kubel-port-forward-pod)
    (define-key map (kbd "l") 'kubel-log-popup)
    (define-key map (kbd "c") 'kubel-copy-popup)
    (define-key map (kbd "e") 'kubel-exec-popup)
    (define-key map (kbd "j") 'kubel-jab-deployment)

    ;; deprecated
    (define-key map (kbd "d") 'kubel-deprecated-warning)
    map)
  "Keymap for `kubel-mode'.")

(defvar kubel-last-position nil)

;;;###autoload
(defun kubel (&optional directory)
  "Invoke the kubel buffer.

DIRECTORY is optional for TRAMP support."
  (interactive)
  (kubel--save-line)
  (kubel--pop-to-buffer (kubel--buffer-name))
  (when directory (setq default-directory directory))
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
  (let ((entries (kubel--populate-list)))
    (setq tabulated-list-format (car entries))
    (setq tabulated-list-entries (cadr entries)))   ; TODO handle "No resource found"
  (setq tabulated-list-sort-key kubel--list-sort-key)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (run-mode-hooks 'kubel-mode-hook))

(add-hook 'kubel-mode-hook #'kubel--jump-back-to-line)

(provide 'kubel)
;;; kubel.el ends here
