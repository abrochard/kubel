;;; kubel.el --- extension for controlling Kubernetes with limited permissions

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
;; Keywords: kubernetes
;; URL: https://github.com/abrochard/kubel
;; License: GNU General Public License >= 3
;; Package-Requires: ((magit "2.90.0") (emacs "26.1"))

;;; Commentary:

;;; Code:

(require 'magit-popup)

(defgroup kubel nil
  "Kubel customization group")

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
  (if (not (get-buffer name))
      (get-buffer-create name))
  (pop-to-buffer name))

(defun kubel--run-command (command &optional buffer-name)
  "Utility function to run a command in a temp buffer.

COMMAND is the command string.
BUFFER-NAME is the name of the temp buffer.  Default to *kubel-command*"
  (if (not buffer-name)
      (setq buffer-name "*kubel-command*"))
  (with-output-to-temp-buffer buffer-name
    (shell-command command
                   buffer-name
                   "*Messages*")
    (pop-to-buffer buffer-name)))

(defun kubel--get-pod-under-cursor ()
  "Utility function to get the name of the pod under the cursor."
  (aref (tabulated-list-get-entry) 0))

(defun kubel--get-command-prefix ()
  "Utility function to prefix the kubectl command with proper context and namespace."
  (concat "kubectl"
          (if (not (equal kubel-context ""))
              (concat " --context " kubel-context))
          (if (not (equal kubel-namespace ""))
              (concat " -n " kubel-namespace))))

(defun kubel--get-containers (pod-name)
  "List the containers in a pod.

POD-NAME is the name of the pod."
  (split-string
   (shell-command-to-string
    (concat (kubel--get-command-prefix)
            " get pod " pod-name
            " -o jsonpath='{.spec.containers[*].name}'")) " "))

;; interactive
(defun kubel-get-pod-details ()
  "Get the dertails of the pod under the cursor."
  (interactive)
  (kubel--run-command (concat
                    (kubel--get-command-prefix)
                    " describe pod " (kubel--get-pod-under-cursor))))

(defun kubel-get-pod-logs ()
  "Get the last N logs of the pod under the cursor."
  (interactive)
  (let* ((containers (kubel--get-containers (kubel--get-pod-under-cursor)))
         (container (car containers)))
    (if (not (equal (length containers) 1))
        (setq container (helm-comp-read "Select container: " containers)))
    (kubel--run-command
     (concat (kubel--get-command-prefix)
             " logs "
             " --tail=" kubel-log-tail-n " "
             (kubel--get-pod-under-cursor) " "
             container
             (if magit-current-popup-args
                 " -f &"))
     (concat "*kubel - logs - " (kubel--get-pod-under-cursor) " - " container "*"))))

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
  "Set the context.  Should remove helm dependency."
  (interactive)
  (setq kubel-context
        (completing-read
         "Select context: "
         (split-string (shell-command-to-string
                        "kubectl config view -o jsonpath='{.contexts[*].name}'")
                       " ")))
  (kubel))

(defun kubel-port-forward-pod (p)
  "Port forward a pod to your local machine.

P is the port as integer."
  (interactive "nPort: ")
  (let ((port (format "%s" p)))
    (kubel--run-command
     (concat (kubel--get-command-prefix) " port-forward "
             (kubel--get-pod-under-cursor) " " port ":" port " &")
     (concat "*kubel - port-forward - " (kubel--get-pod-under-cursor) ":" port "*"))))


;; popups
(magit-define-popup kubel-log-popup
  "Popup for kubel log menu"
  'kubel
  :switches '((?f "Follow" "-f"))
  :options '((?n "lines" kubel-log-tail-n))
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
             (?d "Pod details" kubel-get-pod-details)
             (?C "Set context" kubel-set-context)
             (?n "Set namespace" kubel-set-namespace)
             (?g "Refresh" kubel-mode)
             (?p "Port forward" kubel-port-forward-pod)
             (?l "Logs" kubel-log-popup)
             (?c "Copy" kubel-copy-popup)))

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
