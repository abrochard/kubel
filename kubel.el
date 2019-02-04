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
;; Package-Requires: ((magit-popup "2.4.0") (emacs "25.3"))

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
;; h => help pop up
;; C => set context
;; n => set namespace
;; g => refresh pods
;; p => port forward pod
;; i => describe ingress
;; l => lop popup
;; c => copy popup
;;

;;; Customize:

;; By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.

;;; Code:

(require 'magit-popup)

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

(defvar kubel-namespace ""
  "Current namespace.")
(defvar kubel-context
  (replace-regexp-in-string
   "\n" "" (shell-command-to-string "kubectl config current-context"))
  "Current context.  Tries to smart default.")

(defvar kubel-log-tail-n "100"
  "Number of lines to tail.")

(defun kubel--buffer-name ()
  "Return kubel buffer name."
  (concat "*kubel (" kubel-namespace ") [" kubel-context "]*"))

(defun kubel--list-entries ()
  "Create the entries for the service list."
  (let ((temp (list)))
    (with-temp-buffer
      (insert (shell-command-to-string (concat (kubel--get-command-prefix) " get pods --no-headers=true")))
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-z0-9\-]+\\) +\\([0-9]+/[0-9]+\\) +\\(\\w+\\) +\\([0-9]+\\) +\\([0-9a-z]+\\)$" (point-max) t)
        (setq temp (append temp (list (list "id" (vector (match-string 1) (match-string 2) (match-string 3) (match-string 4) (match-string 5)))))))
      )
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

;; interactive
(defun kubel-get-pod-details ()
  "Get the details of the pod under the cursor."
  (interactive)
  (let* ((pod (kubel--get-pod-under-cursor))
         (buffer-name (format "*kubel - pod - %s*" pod)))
    (kubel--exec buffer-name nil (list "describe" "pod" (kubel--get-pod-under-cursor)))
    (beginning-of-buffer)))

(defun kubel-get-pod-logs ()
  "Get the last N logs of the pod under the cursor."
  (interactive)
  (let* ((pod (kubel--get-pod-under-cursor))
         (containers (kubel--get-containers pod))
         (container (if (equal (length containers) 1)
                        (car containers)
                      (completing-read "Select container: " containers)))
         (buffer-name (format "*kubel - logs - %s - %s*" pod container))
         (async nil))
    (when magit-current-popup-args
      (setq async t))
    (kubel--exec buffer-name async (remove nil (list "logs" (format  "--tail=%s" kubel-log-tail-n) pod container
                                                     (when magit-current-popup-args "-f"))))))

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

(defun kubel-set-namespace (namespace)
  "Set the namespace.

NAMESPACE is the namespace."
  (interactive "MNamespace: ")
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

(defun kubel-describe-ingress ()
  "Show the ingress details."
  (interactive)
  (kubel--exec "*kubel - ingress*" nil (list "describe" "ingress"))
  (beginning-of-buffer))


(defun kubel-describe-service ()
  "Descibe a service."
  (interactive)
  (let* ((cmd (concat (kubel--get-command-prefix)  " get services -o=jsonpath='{.items[*].metadata.name}'"))
         (service (completing-read "Service: " (split-string (shell-command-to-string cmd) " ")))
         (buffer-name (format "*kubel - service - %s*" service)))
    (kubel--exec buffer-name nil (list "get" "service" service "-o" "yaml"))))

(defun kubel-describe-configmaps ()
  "Describe a configmap."
  (interactive)
  (let* ((cmd (concat (kubel--get-command-prefix) " get configmaps -o=jsonpath='{.items[*].metadata.name}'"))
         (configmap (completing-read "Configmap: " (split-string (shell-command-to-string cmd) " ")))
         (buffer-name (format "*kubel - configmap - %s*" configmap)))
    (kubel--exec buffer-name nil (list "describe" "configmap" configmap))
    (beginning-of-buffer)))

(defun kubel-describe-deployment ()
  "Describe a deployment."
  (interactive)
  (let* ((cmd (concat (kubel--get-command-prefix) " get deployments -o=jsonpath='{.items[*].metadata.name}'"))
         (deployment (completing-read "Deployment: " (split-string (shell-command-to-string cmd) " ")))
         (buffer-name (format "*kubel - deployement - %s*" deployment)))
    (kubel--exec buffer-name nil (list "describe" "deployment" deployment))
    (beginning-of-buffer)))

;; popups
(magit-define-popup kubel-log-popup
  "Popup for kubel log menu"
  'kubel
  :switches '((?f "Follow" "-f"))
  :options '((?n "lines" "100"))
  :actions '("Kubel Log Menu"
             (?l "Tail logs for pod" kubel-get-pod-logs)))


(magit-define-popup kubel-copy-popup
  "Popup for kubel copy menu"
  'kubel
  :actions '("Kubel Copy Menu"
             (?c "Copy pod name" kubel-copy-pod-name)
             (?l "Copy pod log command" kubel-copy-log-command)))

(magit-define-popup kubel-help-popup
  "Popup for kubel menu"
  'kubel
  :actions '("Kubel Menu"
             (? <return> "Pod details" kubel-get-pod-details)
             (?C "Set context" kubel-set-context)
             (?n "Set namespace" kubel-set-namespace)
             (?g "Refresh" kubel-mode)
             (?p "Port forward" kubel-port-forward-pod)
             (?l "Logs" kubel-log-popup)
             (?c "Copy" kubel-copy-popup)
             (?i "Ingress" kubel-describe-ingress)
             (?s "Services" kubel-describe-service)
             (?m "Configmaps" kubel-describe-configmaps)
             (?d "Deployements" kubel-describe-deployment)))

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
    (define-key map (kbd "i") 'kubel-describe-ingress)
    (define-key map (kbd "s") 'kubel-describe-service)
    (define-key map (kbd "m") 'kubel-describe-configmaps)
    (define-key map (kbd "d") 'kubel-describe-deployment)
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
