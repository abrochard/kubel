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
;;
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
;;

;;; Customize:

;; By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.

;;; Code:

(require 'transient)

(defconst kubel--list-format
  [("Name" 50 t)
   ("Ready" 10 t)
   ("Status" 20 t)
   ("Restarts" 10 t)
   ("Age" 15 t)]
  "List format.")

(defconst kubel--list-sort-key
  '("Name" . nil)
  "Sort table on this key.")

(defconst kubel--status-colors
  '(("Running" . "green")
    ("Error" . "red")
    ("Completed" . "yellow")
    ("CrashLoopBackOff" . "red")
    ("Terminating" . "blue"))
  "Associative list of status to color.")

(defvar kubel-namespace ""
  "Current namespace.")
(defvar kubel-context
  (replace-regexp-in-string
   "\n" "" (shell-command-to-string "kubectl config current-context"))
  "Current context.  Tries to smart default.")

(defvar kubel-pod-filter "")

(defvar kubel-log-tail-n "100"
  "Number of lines to tail.")

(defun kubel--buffer-name ()
  "Return kubel buffer name."
  (concat "*kubel (" kubel-namespace ") [" kubel-context "]*"))

(defun kubel--propertize-pod-name (name)
  "Return the pod name in proper font color based on active filter.

NAME is the pod name."
  (if (or (equal kubel-pod-filter "") (string-match-p kubel-pod-filter name))
        name
      (propertize name 'font-lock-face '(:foreground "darkgrey"))))

(defun kubel--propertize-status (status)
  "Return the status in proper font color.

STATUS is the pod status string."
  (let ((pair (cdr (assoc status kubel--status-colors))))
    (if pair
        (propertize status 'font-lock-face `(:foreground ,pair))
      status)))

(defun kubel--list-entries ()
  "Create the entries for the service list."
  (let ((temp (list)))
    (with-temp-buffer
      (insert (shell-command-to-string (concat (kubel--get-command-prefix) " get pods --no-headers=true")))
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-z0-9\-]+\\) +\\([0-9]+/[0-9]+\\) +\\(\\w+\\) +\\([0-9]+\\) +\\([0-9a-z]+\\)$" (point-max) t)
        (setq temp (append temp (list (list (match-string 1)
                                            (vector (kubel--propertize-pod-name (match-string 1))
                                                    (match-string 2)
                                                    (kubel--propertize-status (match-string 3))
                                                    (match-string 4)
                                                    (match-string 5))))))))
    temp))

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

(defun kubel--get-pod-under-cursor ()
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
      (yaml-mode))
    (beginning-of-buffer)))

;; interactive
(defun kubel-get-pod-details ()
  "Get the details of the pod under the cursor."
  (interactive)
  (let* ((pod (kubel--get-pod-under-cursor))
         (buffer-name (format "*kubel - pod - %s*" pod)))
    (kubel--exec buffer-name nil (list "describe" "pod" (kubel--get-pod-under-cursor)))
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
  (let* ((pod (kubel--get-pod-under-cursor))
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
  (kill-new (kubel--get-pod-under-cursor))
  (message "Pod name copied to kill-ring"))

(defun kubel-copy-log-command ()
  "Copy the streaming log command of the pod under the cursor."
  (interactive)
  (kill-new (concat (kubel--get-command-prefix)
                    " logs -f --tail=" kubel-log-tail-n " "
                    (kubel--get-pod-under-cursor)))
  (message "Log command copied to kill-ring"))

(defun kubel-copy-command-prefix ()
  "Copy the kubectl command prefix."
  (interactive)
  (kill-new (kubel--get-command-prefix))
  (message "Command prefix copied to kill-ring"))

(defun kubel-set-namespace (namespace)
  "Set the namespace.

NAMESPACE is the namespace."
  (interactive "MNamespace: ")
  (when (get-buffer (kubel--buffer-name))
    (kill-buffer (kubel--buffer-name)))
  (setq kubel-namespace namespace)
  (kubel))

(defun kubel-set-context ()
  "Set the context."
  (interactive)
  (setq kubel-context
        (completing-read
         "Select context: "
         (split-string (shell-command-to-string "kubectl config view -o jsonpath='{.contexts[*].name}'") " ")))
  (kubel))

(defun kubel-port-forward-pod (p)
  "Port forward a pod to your local machine.

P is the port as integer."
  (interactive "nPort: ")
  (let* ((port (format "%s" p))
         (pod (kubel--get-pod-under-cursor))
         (buffer-name (format "*kubel - port-forward - %s:%s*" pod port)))
    (kubel--exec buffer-name t (list "port-forward" pod (format "%s:%s" port port)))))

(defun kubel-describe-ingress (&optional arg)
  "Show the ingress details.

ARG is the optional param to see yaml."
  (interactive "P")
  (if (or arg (transient-args 'kubel-describe-popup))
      (kubel--describe-resource "ingress" t)
    (kubel--describe-resource "ingress")))


(defun kubel-describe-service (&optional arg)
  "Descibe a service.

ARG is the optional param to see yaml."
  (interactive "P")
  (if (or arg (transient-args 'kubel-describe-popup))
      (kubel--describe-resource "service" t)
    (kubel--describe-resource "service")))

(defun kubel-describe-configmaps (&optional arg)
  "Describe a configmap.

ARG is the optional param to see yaml."
  (interactive "P")
  (if (or arg (transient-args 'kubel-describe-popup))
      (kubel--describe-resource "configmap" t)
    (kubel--describe-resource "configmap")))

(defun kubel-describe-deployment (&optional arg)
  "Describe a deployment.

ARG is the optional param to see yaml."
  (interactive "P")
  (if (or arg (transient-args 'kubel-describe-popup))
      (kubel--describe-resource "deployment" t)
    (kubel--describe-resource "deployment")))

(defun kubel-describe-job (&optional arg)
  "Describe a job.

ARG is the optional param to see yaml."
  (interactive "P")
  (if (or arg (transient-args 'kubel-describe-popup))
      (kubel--describe-resource "job" t)
    (kubel--describe-resource "job")))

(defun kubel-exec-pod ()
  "Kubectl exec into the pod under the cursor."
  (interactive)
  (let* ((pod (kubel--get-pod-under-cursor))
         (containers (kubel--get-containers pod))
         (container (if (equal (length containers) 1)
                        (car containers)
                      (completing-read "Select container: " containers))))
    (eshell)
    (insert (format "%s exec -it %s -c %s /bin/sh" (kubel--get-command-prefix) pod container))))

(defun kubel-delete-pod ()
  "Kubectl delete pod under cursor."
  (interactive)
  (let* ((pod (kubel--get-pod-under-cursor))
         (buffer-name (format "*kubel - delete pod -%s" pod))
         (args (list "delete" "pod" pod)))
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

(define-transient-command kubel-help-popup ()
  "Kubel Menu"
  ["Actions"
   ("ENTER" "Pod details" kubel-get-pod-details)
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
   ("f" "Filter" kubel-set-filter)])

;; mode map
(defvar kubel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'kubel-get-pod-details)
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
  (setq tabulated-list-format kubel--list-format)
  (setq tabulated-list-entries 'kubel--list-entries)
  (setq tabulated-list-sort-key kubel--list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (run-mode-hooks 'kubel-mode-hook))

(provide 'kubel)
;;; kubel.el ends here
