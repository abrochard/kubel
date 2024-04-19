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
;; C-u enter => describe resource
;; h => help popup
;; ? => help popup
;; E => quick edit any resource
;; g => refresh
;; k => delete popup
;; r => see the rollout history for resource
;; p => port forward pod
;; l => log popup
;; e => exec popup
;; j => jab deployment to force rolling update
;; S => scale replicas
;; C => set context
;; n => set namespace
;; R => set resource
;; K => set kubectl config file
;; F => set output format
;; f => set a substring filter for resource name
;; M-n => jump to the next highlighted resource
;; M-p => jump to previous highlighted resource
;; m => mark item
;; u => unmark item
;; M => mark all items
;; U => unmark all items
;; c => copy popup
;; $ => show process buffer
;; s => show only resources with specified label value

;;; Customize:

;; By default, kubel log tails from the last 100 lines, you can change the `kubel-log-tail-n` variable to set another line number.

;;; Code:

(require 'transient)
(require 'dash)
(require 's)
(require 'yaml-mode)
(require 'tramp)
(require 'subr-x)
(require 'eshell)
(require 'dired)
(require 'json)

(defgroup kubel nil "Customisation group for kubel."
  :group 'extensions)

(defconst kubel--list-sort-key
  '("NAME" . nil)
  "Sort table on this key.")

(defface kubel-status-running
  '((default . (:inherit success)))
  "The face to use for the Running status.")

(defface kubel-status-healthy
  '((default . (:inherit success)))
  "The face to use for the Healthy status.")

(defface kubel-status-active
  '((default . (:inherit success)))
  "The face to use for the Active status.")

(defface kubel-status-ready
  '((default . (:inherit success)))
  "The face to use for the Ready status.")

(defface kubel-status-true
  '((default . (:inherit success)))
  "The face to use for the True status.")

(defface kubel-status-unknown
  '((default . (:inherit warning)))
  "The face to use for the Unknown status.")

(defface kubel-status-error
  '((default . (:inherit error)))
  "The face to use for the Error status.")

(defface kubel-status-evicted
  '((default . (:inherit error)))
  "The face to use for the Evicted status.")

(defface kubel-status-memory-pressure
  '((default . (:inherit error)))
  "The face to use for the Memory Pressure status.")

(defface kubel-status-pid-pressure
  '((default . (:inherit error)))
  "The face to use for the PID Pressure status.")

(defface kubel-status-disk-pressure
  '((default . (:inherit error)))
  "The face to use for the Disk Pressure status.")

(defface kubel-status-revision-missing
  '((default . (:inherit error)))
  "The face to use for the Revision Missing status.")

(defface kubel-status-revision-failed
  '((default . (:inherit error)))
  "The face to use for the Revision Failed status.")

(defface kubel-status-network-unavailable
  '((default . (:inherit error)))
  "The face to use for the Network Unavailable status.")

(defface kubel-status-completed
  '((default . (:foreground "yellow")))
  "The face to use for the Completed status.")

(defface kubel-status-crash-loop-backoff
  '((default . (:inherit error)))
  "The face to use for the Crash Loop Backoff status.")

(defface kubel-status-terminating
  '((default . (:foreground "blue")))
  "The face to use for the Terminating status.")

(defcustom kubel-status-faces
  '(("Running" . kubel-status-running)
    ("Healthy" . kubel-status-healthy)
    ("Active" . kubel-status-active)
    ("Ready" . kubel-status-ready)
    ("True" . kubel-status-true)
    ("Unknown" . kubel-status-unknown)
    ("Error" . kubel-status-error)
    ("Evicted" . kubel-status-evicted)
    ("MemoryPressure" . kubel-status-memory-pressure)
    ("PIDPressure" . kubel-status-pid-pressure)
    ("DiskPressure" . kubel-status-disk-pressure)
    ("RevisionMissing" . kubel-status-revision-missing)
    ("RevisionFailed" . kubel-status-revision-failed)
    ("NetworkUnavailable" . kubel-status-network-unavailable)
    ("Completed" . kubel-status-completed)
    ("CrashLoopBackOff" . kubel-status-crash-loop-backoff)
    ("Terminating" . kubel-status-terminating))
  "Associative list of status to face."
  :type '(alist :key-type string
                :value-type face)
  :group 'kubel)

(defcustom kubel-kubectl "kubectl"
  "Kubectl binary path."
  :type '(file :must-match t)
  :group 'kubel)

(defconst kubel--process-buffer "*kubel-process*"
  "Kubel process buffer name.")

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

auto - default, use `kubectl auth can-i list namespace` to determine if we can
       list namespaces
on   - always assume we can list namespaces
off  - always assume we cannot list namespaces"
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "On" on)
                 (const :tag "Off" off))
  :group 'kubel)

(defcustom kubel-shell-buffer-name-format "kubel:%C:%n:%t:%c@%p"
  "Define the name format used for pod shell buffers.

This is a format string with %-sequences that will be substituted
with information about the shell's connection. The following
%-sequences are defined:

%t: The shell type. Examples of this are `shell` and `eshell`
%c: The container name
%p: The pod name
%n: The current namespace
%C: The current context"
  :type 'string
  :group 'kubel)

(defun kubel--append-to-process-buffer (str)
  "Append string STR to the process buffer."
  (with-current-buffer (get-buffer-create kubel--process-buffer)
    (read-only-mode -1)
    (goto-char (point-max))
    (insert (format "%s\n" str))))

(defvar-local kubel--last-command nil)

(defun kubel--log-command (process-name cmd)
  "Log the kubectl command to the process buffer.

PROCESS-NAME is the name of the process.
CMD is the kubectl command as a list."
  (let ((str-cmd (if (equal 'string (type-of cmd)) cmd (mapconcat #'identity cmd " "))))
    (setq kubel--last-command str-cmd)
    (kubel--append-to-process-buffer
     (format "[%s]\ncommand: %s" process-name str-cmd))))

(defun kubel--exec-to-string (cmd)
  "Replace \"shell-command-to-string\" to log to process buffer.

CMD is the command string to run."
  (kubel--log-command "kubectl-command" cmd)
  (with-output-to-string
    (with-current-buffer standard-output
      (shell-command cmd t "*kubel stderr*"))))

(defvar-local kubel-namespace "default"
  "Current namespace.")

(defvar-local kubel-resource "Pods"
  "Current resource.")

(defvar-local kubel-context
  (replace-regexp-in-string
   "\n" "" (kubel--exec-to-string "kubectl config current-context"))
  "Current context.  Tries to smart default.")

(defvar-local kubel-resource-filter ""
  "Substring filter for resource name.")

(defvar-local kubel-selector ""
  "Label selector for resources.")

(defvar kubel-namespace-history '()
  "List of previously used namespaces.")

(defvar kubel-selector-history '()
  "List of previously used selectors.")

(defvar-local kubel--can-get-namespace-cached nil)

(defvar kubel--namespace-list-cached nil)

(defvar-local kubel--label-values-cached nil)

(defvar-local kubel--selected-items '())

(defvar-local kubel--kubernetes-resources-list-cached nil)

(defun kubel--kubernetes-resources-list ()
  "Get list of resources from cache or from fetching the api resource."
  (if (null kubel--kubernetes-resources-list-cached)
      (setq kubel--kubernetes-resources-list-cached
            (kubel--fetch-api-resource-list))
    kubel--kubernetes-resources-list-cached))

(defun kubel--invalidate-context-caches ()
  "Invalidate the context caches."
  (setq kubel--kubernetes-resources-list-cached nil)
  (setq kubel--can-get-namespace-cached nil)
  (setq kubel--namespace-list-cached nil)
  (setq kubel--label-values-cached nil))

(defun kubel--populate-list ()
  "Return a list with a tabulated list format and \"tabulated-list-entries\"."
  (let*  ((body (kubel--exec-to-string (concat (kubel--get-command-prefix) " get " kubel-resource)))
          (entrylist (kubel--parse-body body)))
    (when (string-prefix-p "No resources found" body)
      (message "No resources found"))  ;; TODO exception here
    (list (kubel--get-list-format entrylist) (kubel--get-list-entries entrylist))))

(defun kubel--age-to-secs (age)
  "Convert AGE in format 1d2h3m4s to seconds."
  (let ((rex (rx bol
                 (opt (group (one-or-more digit)) "d")
                 (opt (group (one-or-more digit)) "h")
                 (opt (group (one-or-more digit)) "m")
                 (opt (group (one-or-more digit)) "s")
                 eol)))
    (if (string-match rex age)
        (-sum (--map-indexed
               (* (--if-let (match-string (1+ it-index) age)
                      (string-to-number it)
                    0)
                  it)
               '(86400 3600 60 1)))
      0)))

(defun kubel--make-age-comparator (colnum)
  "Return a function that compares two ages at given column COLNUM."
  (lambda (row1 row2)
    (let ((age1 (elt (cadr row1) colnum))
          (age2 (elt (cadr row2) colnum)))
      (< (kubel--age-to-secs age1)
         (kubel--age-to-secs age2)))))

(defun kubel--column-entry (entrylist)
  "Return a function of colnum to retrieve an entry in a given column for ENTRYLIST."
  (function
   (lambda (colnum)
     (let* ((name (kubel--column-header entrylist colnum))
            (width (+ 4 (kubel--column-width entrylist colnum)))
            (sort (if (member name '("AGE" "DURATION" "LAST SCHEDULE"))
                      (kubel--make-age-comparator colnum)
                    t)))
       (list name width sort)))))

(defun kubel--get-list-format (entrylist)
  "Get the list format.

ENTRYLIST is the output of the parsed body."
  (defun kubel--get-column-entry (colnum)
    (let ((kubel--get-entry (kubel--column-entry entrylist)))
      (funcall kubel--get-entry colnum)))
  (cl-map 'vector #'kubel--get-column-entry (number-sequence 0 (- (kubel--ncols entrylist) 1))))

(defun kubel--update-selected-items (entries)
  "Check that all selected items still exist.

ENTRIES are all resources."
  (dolist (i (-difference kubel--selected-items (mapcar #'car entries)))
    (setq kubel--selected-items (delete i kubel--selected-items))))

(defun kubel--get-list-entries (entrylist)
  "Get the entries.

ENTRYLIST is the output of the parsed body."
  (let ((entries (cdr entrylist)))
    (kubel--update-selected-items entries)
    (mapcar (lambda (x)
              (list (car x)
                    (vconcat [] (mapcar #'kubel--propertize-status x))))
            entries)))

(defun kubel--parse-body (body)
  "Parse the body of kubectl get resource call into a list.

BODY is the raw output of kubectl get resource."
  (let* ((lines (or (nbutlast (split-string body "\n")) '("")))
         (header (car lines))
         ;; Cronjobs have a "LAST SCHEDULE" column, so need to split on 2+ whitespace chars.
         (starts (cl-loop for start = 0 then (match-end 0)
                          while (string-match (rx (>= 2 whitespace)) header start)
                          collect (match-end 0)))
         (position (-zip-with 'cons (cons 0 starts) (append starts '("end"))))
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

(defun kubel--buffer-name-from-parameters (context namespace resource)
  "Return a preconfigured kubel buffer name."
  (concat (format "*kubel:%s:%s:%s*" context namespace resource)))

(defun kubel--buffer-name ()
  "Return kubel buffer name."
  (concat (kubel--buffer-name-from-parameters kubel-context kubel-namespace kubel-resource)
          (unless (equal kubel-selector "")
            (format " (%s)" kubel-selector))))

(defun kubel--items-selected-p ()
  "Return non-nil if there are items selected."
  (>= (length kubel--selected-items) 1))

(defun kubel--propertize-status (status)
  "Return the status in proper font color.

STATUS is the pod status string."
  (let ((status-face (cdr (assoc status kubel-status-faces)))
        (match (or (equal kubel-resource-filter "") (string-match-p kubel-resource-filter status)))
        (selected (and (kubel--items-selected-p) (-contains? kubel--selected-items status))))
    (cond (status-face (propertize status 'face status-face))
          (selected (propertize (concat "*" status) 'face 'dired-marked))
          ((not match) (propertize status 'face 'shadow))
          (t status))))

(defun kubel--pop-to-buffer (name)
  "Utility function to pop to buffer or create it.

NAME is the buffer name."
  (unless (get-buffer name)
    (get-buffer-create name))
  (pop-to-buffer-same-window name))

(defun kubel--process-error-buffer (process-name)
  "Return the error buffer name for the PROCESS-NAME."
  (format "*%s:err*" process-name))

(defun kubel--sentinel (callback)
  "Sentinel function used by KUBEL--EXEC.

CALLBACK is called when process completes successfully.
"
  (lambda (process event)
    (let ((process-name (process-name process))
          (exit-status (process-exit-status process)))
      (kubel--append-to-process-buffer (format "[%s]\nexit-code: %s" process-name exit-status))
      (if (eq 0 exit-status)
          (when callback (funcall callback))
        (let ((err (with-current-buffer (kubel--process-error-buffer process-name)
                     (buffer-string))))
          (kubel--append-to-process-buffer (format "error: %s" err))
          (error (format "Kubel process %s error: %s" process-name err)))))))

(defun kubel--exec (process-name args &optional readonly callback)
  "Utility function to run commands in the proper context and namespace.

PROCESS-NAME is an identifier for the process.  Default to \"kubel-command\".
ARGS is a ist of arguments.
CALLBACK is a function that will be executed when the command completes.
READONLY If true buffer will be in readonly mode(view-mode)."
  (when (equal process-name "")
    (setq process-name "kubel-command"))
  (let ((buffer-name (format "*kubel-resource:%s:%s:%s*" kubel-context kubel-namespace (string-join args "_")))
        (error-buffer (kubel--process-error-buffer process-name))
        (cmd (append (list kubel-kubectl) (kubel--get-context-namespace) args)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (when (get-buffer error-buffer)
      (kill-buffer error-buffer))
    (kubel--log-command process-name cmd)
    (make-process :name process-name
                  :buffer buffer-name
                  :sentinel (kubel--sentinel callback)
                  :file-handler t
                  :stderr error-buffer
                  :command cmd)
    (pop-to-buffer buffer-name)
    (if readonly
        (with-current-buffer buffer-name
          (view-mode)))))

(defun kubel--get-resource-under-cursor ()
  "Utility function to get the name of the resource under the cursor.
Strip the `*` prefix if the resource is selected"
  (string-remove-suffix " (default)" ;; see https://github.com/abrochard/kubel/issues/106
                        (replace-regexp-in-string
                         "^\*" "" (aref (tabulated-list-get-entry) 0))))

(defun kubel--get-context-namespace ()
  "Utility function to return the proper context and namespace arguments."
  (append
   (unless (equal kubel-context "")
     (list "--context" kubel-context))
   (unless (equal kubel-namespace "default")
     (list "-n" kubel-namespace))))

(defun kubel--get-selector ()
  "Utility function to return current label selector."
  (unless (equal kubel-selector "")
    (list "--selector" kubel-selector)))

(defun kubel--get-command-prefix ()
  "Utility function to prefix the kubectl command with proper context and namespace."
  (mapconcat 'identity (append (list kubel-kubectl) (kubel--get-context-namespace) (kubel--get-selector)) " "))

(defun kubel--get-containers (pod-name &optional type)
  "List the containers in a pod.

POD-NAME is the name of the pod.
TYPE is containers or initContainers."
  (unless type (setq type "containers"))
  (split-string
   (kubel--exec-to-string
    (format "%s get pod %s -o jsonpath='{.spec.%s[*].name}'" (kubel--get-command-prefix) pod-name type)) " "))

(defun kubel--get-pod-labels ()
  "List labels of pods in a current namespace."
  (let* ((raw-labels
          (split-string
           (replace-regexp-in-string
            (regexp-quote ":") "="
            (replace-regexp-in-string
             "map\\[\\(.+?\\)\\]" "\\1"
             (kubel--exec-to-string
              (format "%s get pod -o jsonpath='{.items[*].metadata.labels}'" (kubel--get-command-prefix)))))))
         (splitted (mapcan (lambda (s) (split-string s ","))
                           raw-labels))
         (cleaned (mapcar (lambda (s) (replace-regexp-in-string "[{|\"|}]" "" s)) splitted))
         (unique (-distinct cleaned)))
    unique))

(defun kubel--select-resource (name)
  "Prompt user to select an instance out of a list of resources.

NAME is the string name of the resource."
  (let ((cmd (format "%s get %s -o=jsonpath='{.items[*].metadata.name}'"
                     (kubel--get-command-prefix) name)))
    (completing-read (concat (s-upper-camel-case name) ": ")
                     (split-string (kubel--exec-to-string cmd) " "))))

(defun kubel--describe-resource (name &optional describe)
  "Describe a specific resource.

NAME is the string name of the resource to decribe.
DESCRIBE is boolean to describe instead of get resource details"
  (let* ((resource (kubel--select-resource name))
         (process-name (format "kubel - %s - %s" name resource))
         (callback (lambda () (goto-char (point-min)))))
    (if describe
        (kubel--exec process-name (list "describe" name resource) nil callback)
      (kubel--exec process-name (list "get" name "-o" kubel-output resource) nil callback))
    (when (string-equal kubel-output "yaml")
      (yaml-mode)
      (kubel-yaml-editing-mode))))

(defun kubel--show-rollout-revision (type name)
  "Show a specific revision of a certain resource.

TYPE is the resource type.
NAME is the resource name."
  (let* ((typename (format "%s/%s" type name))
         (revision (car (split-string (kubel--select-rollout typename))))
         (process-name (format "kubel - rollout - %s - %s" typename revision))
         (callback (goto-char (point-min))))
    (kubel--exec process-name
                 (list "rollout" "history" typename (format "--revision=%s" revision)) nil callback)))

(defun kubel--list-rollout (typename)
  "Return a list of revisions with format '%number   %cause'.

TYPENAME is the resource type/name."
  (let ((cmd (format "%s rollout history %s" (kubel--get-command-prefix) typename)))
    (nthcdr 2 (split-string (kubel--exec-to-string cmd) "\n" t))))

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
  "Return non-nil if this is a deployment view."
  (-contains? '("Deployments" "deployments" "deployments.apps") kubel-resource))

(defun kubel--is-scalable ()
  "Return non-nil if the resource can be scaled."
  (or
   (kubel--is-deployment-view)
   (-contains? '("ReplicaSets" "replicasets" "replicasets.apps") kubel-resource)
   (-contains? '("StatefulSets" "statefulsets" "statefulsets.apps") kubel-resource)))

;; interactive
;;;###autoload
(define-minor-mode kubel-yaml-editing-mode
  "Kubel Yaml editing mode.
Use C-c C-c to kubectl apply the current yaml buffer."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'kubel-apply)
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
      (kubel--exec (format "kubectl - apply - %s" filename) (list "apply" "-f" filename-without-tramp-prefix) nil (lambda () (message "Applied %s" filename))))))

(defun kubel-get-resource-details (&optional describe)
  "Get the details of the resource under the cursor.

 DESCRIBE is the optional param to describe instead of get."
  (interactive "P")
  (let* ((resource (kubel--get-resource-under-cursor))
         (ctx kubel-context)
         (ns kubel-namespace)
         (res kubel-resource)
         (process-name (format "kubel - %s - %s" kubel-resource resource))
         (callback (lambda () (goto-char (point-min)))))
    (if describe
        (kubel--exec process-name (list "describe" kubel-resource (kubel--get-resource-under-cursor)) nil callback)
      (kubel--exec process-name (list "get" kubel-resource (kubel--get-resource-under-cursor) "-o" kubel-output) nil callback))
    (when (or (string-equal kubel-output "yaml") (transient-args 'kubel-describe-popup))
      (yaml-mode)
      (kubel-yaml-editing-mode)
      (setq kubel-context ctx)
      (setq kubel-namespace ns)
      (setq kubel-resource res))))

(defun kubel--default-tail-arg (args)
  "Ugly function to make sure that there is at least the default tail.

ARGS is the arg list from transient."
  (if (car (remove nil (mapcar (lambda (x)
                                 (string-prefix-p "--tail=" x)) args)))
      args
    (append args (list (concat "--tail=" (format "%s" kubel-log-tail-n))))))

(defun kubel-get-pod-logs (&optional args type)
  "Get the last N logs of the pod under the cursor.

ARGS is the arguments list from transient.
TYPE is containers or initContainers."
  (interactive
   (list (transient-args 'kubel-log-popup)))
  (dolist (pod (if (kubel--is-pod-view)
                   (if (kubel--items-selected-p)
                       kubel--selected-items
                     (list (kubel--get-resource-under-cursor)))
                 (list (kubel--select-resource "Pods"))))
    (let* ((type (or type "containers"))
           (containers (kubel--get-containers pod type))
           (container (if (equal (length containers) 1)
                          (car containers)
                        (completing-read "Select container: " containers)))
           (process-name (format "kubel - logs - %s - %s" pod container)))
      (kubel--exec process-name
                   (append '("logs") (kubel--default-tail-arg args) (list pod container)) t nil))))

(defun kubel-get-pod-logs--initContainer (&optional args)
  "Get the last N logs of the pod under the cursor.

ARGS is the arguments list from transient."
  (interactive
   (list (transient-args 'kubel-log-popup)))
  (kubel-get-pod-logs args "initContainers"))

(defun kubel-get-logs-by-labels (&optional args)
  "Get the last N logs of the pods by labels.
ARGS is the arguments list from transient."
  (interactive
   (list (transient-args 'kubel-log-popup)))
  (let* ((labels (kubel--get-pod-labels))
         (label (completing-read "Select container: " labels))
         (process-name (format "kubel - logs - %s" label)))
    (kubel--exec process-name
                 (append '("logs") (kubel--default-tail-arg args) '("-l") (list label)) t nil)))

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

(defun kubel-copy-last-command ()
  "Copy the last kubectl command ran."
  (interactive)
  (kill-new kubel--last-command)
  (message (concat "Last command copied: " kubel--last-command)))

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
                   (string-match-p "yes\n"
                                   (kubel--exec-to-string
                                    (format "%s --context %s auth can-i list namespaces" kubel-kubectl kubel-context))))))
         kubel--can-get-namespace-cached)))

(defun kubel--get-namespace ()
  "Get namespaces for current context, try to recover from cache first."
  (unless kubel--namespace-list-cached
    (setq kubel--namespace-list-cached
          (split-string (kubel--exec-to-string
                         (format "%s --context %s get namespace -o jsonpath='{.items[*].metadata.name}'" kubel-kubectl kubel-context)) " ")))
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
    (with-current-buffer (clone-buffer)
      (setq kubel-namespace namespace)
      (kubel--add-namespace-to-history namespace)
      (switch-to-buffer (current-buffer))
      (kubel-refresh last-default-directory))))

(defun kubel-set-context ()
  "Set the context."
  (interactive)
  (let* ((kubel--buffer (get-buffer (kubel--buffer-name)))
         (last-default-directory (when kubel--buffer (with-current-buffer kubel--buffer default-directory))))
    (with-current-buffer (clone-buffer)
      (setq kubel-context
            (completing-read
             "Select context: "
             (split-string (kubel--exec-to-string (format "%s config view -o jsonpath='{.contexts[*].name}'" kubel-kubectl)) " ")))
      (kubel--invalidate-context-caches)
      (setq kubel-namespace "default")
      (switch-to-buffer (current-buffer))
      (kubel-refresh last-default-directory))))

(defun kubel--add-selector-to-history (selector)
  "Add SELECTOR to history if it isn't there already."
  (unless (member selector kubel-selector-history)
    (push selector kubel-selector-history)))

(defun kubel--get-all-selectors ()
  "Get all selectors."
  (unless kubel--label-values-cached
    (let ((labels (kubel--get-pod-labels)))
      (setq kubel--label-values-cached labels)))
  kubel--label-values-cached)

(defun kubel--list-selectors ()
  "List selector expressions from history."
  (delete-dups
   (append '("none") (kubel--get-all-selectors)
           kubel-selector-history)))

(defun kubel-set-label-selector ()
  "Set the selector."
  (interactive)
  (with-current-buffer (clone-buffer)
    (let ((selector (completing-read
                     "Selector: "
                     (kubel--list-selectors))))
      (when (equal selector "none")
        (setq selector ""))
      (setq kubel-selector selector))
    (kubel--add-selector-to-history kubel-selector)
    ;; Update pod list according to the label selector
    (switch-to-buffer (current-buffer))
    (kubel-refresh)))

(defun kubel--fetch-api-resource-list ()
  "Fetch the API resource list."
  (split-string (kubel--exec-to-string
                 (format "%s --context %s api-resources -o name --no-headers=true" kubel-kubectl kubel-context)) "\n" t))

(defun kubel-set-resource (&optional refresh)
  "Set the resource.
If called with a prefix argument REFRESH, refreshes
the context caches, including the cached resource list."
  (interactive "P")
  (when refresh (kubel--invalidate-context-caches))
  (let* ((current-buffer-name (kubel--buffer-name))
         (resource-list (kubel--kubernetes-resources-list))
         (kubel--buffer (get-buffer current-buffer-name))
         (last-default-directory (when kubel--buffer (with-current-buffer kubel--buffer default-directory))))
    (with-current-buffer (clone-buffer)
      (setq kubel-resource
            (completing-read "Select resource: " resource-list))
      (switch-to-buffer (current-buffer))
      (kubel-refresh last-default-directory))))

(defun kubel-set-output-format ()
  "Set output format of kubectl."
  (interactive)
  (setq kubel-output
        (completing-read
         "Set output format: "
         '("yaml" "json" "wide" "custom-columns="))))

(defun kubel-port-forward-pod (p)
  "Port forward a pod to your local machine.

P can be a single number or a localhost:container port pair."
  (interactive "sPort: ")
  (let* ((port (if (string-match-p ":" p) p (format "%s:%s" p p)))
         (pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (process-name (format "kubel - port-forward - %s:%s" pod port)))
    (kubel--exec process-name (list "port-forward" pod port))))

(defun kubel-setup-tramp ()
  "Setup a kubectl TRAMP."
  (setq tramp-methods (delete (assoc "kubectl" tramp-methods) tramp-methods)) ;; cleanup previous tramp method
  ;; TODO error message if resource is not pod
  (add-to-list 'tramp-methods
               `("kubectl"
                 (tramp-login-program      ,kubel-kubectl)
                 (tramp-login-args         (,(kubel--get-context-namespace) ("exec" "-it") ("-c" "%u") ("%h") ("sh")))
                 (tramp-remote-shell       "sh")
                 (tramp-remote-shell-args  ("-i" "-c"))))) ;; add the current context/namespace to tramp methods

(defun kubel--get-container-under-cursor ()
  "Get `(container . pod)' name under cursor."
  (let* ((pod (if (kubel--is-pod-view)
                  (kubel--get-resource-under-cursor)
                (kubel--select-resource "Pods")))
         (containers (kubel--get-containers pod))
         (container (if (equal (length containers) 1)
                        (car containers)
                      (completing-read "Select container: " containers))))
    (cons container pod)))

(defun kubel--dir-prefix ()
  "Return the current directory prefix for a TRAMP connection."
  (or
   (when (tramp-tramp-file-p default-directory)
     (with-parsed-tramp-file-name default-directory nil
       (format "%s%s:%s@%s|" (or hop "") method user host)))
   ""))

(defun kubel-exec-pod ()
  "Exec into the pod under the cursor -> `find-file."
  (interactive)
  (kubel-setup-tramp)
  (let* ((dir-prefix (kubel--dir-prefix))
         (con-pod (kubel--get-container-under-cursor)))
    (find-file (format "/%skubectl:%s@%s:/" dir-prefix (car con-pod) (cdr con-pod)))))

(defun kubel--shell-buffer-name (shell-type container pod)
  "Generate the name for a pod's shell buffer.

This uses `kubel-shell-buffer-name-format' as the buffer name
format. See the documentation for it for more information on how
to set this format.

The values for the current namespace and context are pulled from
the variables `kubel-namespace' and `kubel-context', respectively."
  (format-spec kubel-shell-buffer-name-format
               `((?t . ,shell-type)
                 (?c . ,container)
                 (?p . ,pod)
                 (?n . ,kubel-namespace)
                 (?C . ,kubel-context))))

(defun kubel-exec-shell-pod ()
  "Exec into the pod under the cursor -> shell."
  (interactive)
  (kubel-setup-tramp)
  (let* ((dir-prefix (kubel--dir-prefix))
         (con-pod (kubel--get-container-under-cursor))
         (container (car con-pod))
         (pod (cdr con-pod))
         (default-directory (format "/%skubectl:%s@%s:/" dir-prefix container pod)))
    (shell (kubel--shell-buffer-name "shell" container pod))))

(defun kubel-exec-eshell-pod ()
  "Exec into the pod under the cursor -> eshell."
  (interactive)
  (kubel-setup-tramp)
  (let* ((dir-prefix (kubel--dir-prefix))
         (con-pod (kubel--get-container-under-cursor))
         (container (car con-pod))
         (pod (cdr con-pod))
         (default-directory (format "/%skubectl:%s@%s:/" dir-prefix container pod))
         (eshell-buffer-name
          (kubel--shell-buffer-name "eshell" container pod)))
    (eshell)))

(defun kubel-exec-vterm-pod ()
  "Exec into the pod under the cursor -> vterm."
  (interactive)
  (kubel-setup-tramp)
  (let* ((dir-prefix (kubel--dir-prefix))
         (con-pod (kubel--get-container-under-cursor))
         (container (car con-pod))
         (pod (cdr con-pod))
         (default-directory (format "/%skubectl:%s@%s:/" dir-prefix container pod))
         (vterm-buffer-name
          (kubel--shell-buffer-name "vterm" container pod))
         (vterm-shell "/bin/sh"))
    (vterm)))

;;;###autoload
(defun kubel-vterm-setup ()
  "Adds a vterm enty to the KUBEL-EXEC-POP."
  (require 'vterm)
  (transient-append-suffix 'kubel-exec-popup "e"
    '("v" "Vterm" kubel-exec-vterm-pod)))

(defun kubel-exec-ansi-term-pod ()
  "Exec into the pod under the cursor -> `ansi-term'."
  (interactive)
  (let* ((con-pod (kubel--get-container-under-cursor))
         (container (car con-pod))
         (pod (cdr con-pod))
         (command (format "%s exec %s -c %s -i -t -- /usr/bin/env sh" (kubel--get-command-prefix) pod container)))
    (with-current-buffer (ansi-term "bash" (kubel--shell-buffer-name "ansi-term" container pod))
      (process-send-string (current-buffer) (format "%s\n" command)))))

(defun kubel-exec-pod-by-shell-command ()
  "Prompt shell with kubectl exec command at pod under cursor."
  (interactive)
  (kubel-setup-tramp)
  (let* ((con-pod (kubel--get-container-under-cursor))
         (command (read-string "Shell command: " (format "%s exec %s -c %s -- " (kubel--get-command-prefix) (cdr con-pod) (car con-pod)))))
    (shell-command command)))

(defun kubel-delete-resource ()
  "Kubectl delete resource under cursor."
  (interactive)
  (dolist (pod (if (kubel--items-selected-p)
                   kubel--selected-items
                 (list (kubel--get-resource-under-cursor))))
    (let* ((process-name (format "kubel - delete %s - %s" kubel-resource pod))
           (args (list "delete" kubel-resource pod)))
      (when (transient-args 'kubel-delete-popup)
        (setq args (append args (list "--force" "--grace-period=0"))))
      (kubel--exec process-name args))))

(defun kubel-jab-deployment ()
  "Make a trivial patch to force a new deployment.

See https://github.com/kubernetes/kubernetes/issues/27081"
  (interactive)
  (dolist (deployment (if (kubel--is-deployment-view)
                          (if (kubel--items-selected-p)
                              kubel--selected-items
                            (list (kubel--get-resource-under-cursor)))
                        (list (kubel--select-resource "Deployments"))))
    (let ((process-name (format "kubel - bouncing - %s" deployment)))
      (kubel--exec process-name (list "patch" "deployment" deployment "-p"
                                      (format "{\"spec\":{\"template\":{\"metadata\":{\"labels\":{\"date\":\"%s\"}}}}}"
                                              (round (time-to-seconds))))))))

(defun kubel-scale-replicas (replicas)
  "Scale resource replicas.

REPLICAS is the number of desired replicas."
  (interactive (list (read-number "Replicas: ")))
  (if (kubel--is-scalable)
      (let* ((resource (kubel--get-resource-under-cursor))
             (process-name (format "kubel:scale:%s/%s" kubel-resource resource)))
        (kubel--exec process-name (list "scale" kubel-resource resource "--replicas" (number-to-string replicas))))
    (message
     "[%s] cannot be scaled.\nOnly these resources can be scaled: [deployment, replica set, replication controller, and stateful set]."
     kubel-resource)))

(defun kubel-set-filter (filter)
  "Set the pod filter.

FILTER is the filter string."
  (interactive "MFilter: ")
  (setq kubel-resource-filter filter)
  (kubel-refresh))

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

(defun kubel-quick-edit ()
  "Quickly edit any resource."
  (interactive)
  (kubel--describe-resource
   (completing-read "Select resource: " (kubel--kubernetes-resources-list))))

(defun kubel-show-process-buffer ()
  "Show the kubel-process-buffer."
  (interactive)
  (pop-to-buffer kubel--process-buffer)
  (special-mode))

(defun kubel-mark-item ()
  "Mark or unmark the item under cursor."
  (interactive)
  (let ((item (kubel--get-resource-under-cursor)))
    (unless (-contains? kubel--selected-items item)
      (progn
        (push item kubel--selected-items)
        (forward-line 1)
        (kubel-refresh)))))

(defun kubel-unmark-item ()
  "Unmark the item under cursor."
  (interactive)
  (let ((item (kubel--get-resource-under-cursor)))
    (when (-contains? kubel--selected-items item)
      (progn
        (setq kubel--selected-items (delete item kubel--selected-items))
        (kubel-refresh)))))

(defun kubel-mark-all ()
  "Mark all items."
  (interactive)
  (setq kubel--selected-items '())
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (push (kubel--get-resource-under-cursor) kubel--selected-items)
      (forward-line 1)))
  (kubel-refresh))

(defun kubel-unmark-all ()
  "Unmark all items."
  (interactive)
  (setq kubel--selected-items '())
  (kubel-refresh))

(defun kubel--read-buffer ()
  "Return the list of all buffers of kubel pattern."
  (let* ((other-buffer (other-buffer (current-buffer)))
         (other-name (buffer-name other-buffer))
         (buffers))
    (dolist (buf (buffer-list))
      (when (string-prefix-p "*kubel:" (buffer-name buf))
        (push buf buffers)))
    (let ((predicate
           (lambda (buffer)
             ;; BUFFER is an entry (BUF-NAME . BUF-OBJ) of Vbuffer_alist.
             (memq (cdr buffer) buffers))))
      (read-buffer
       "Switch to buffer: "
       (when (funcall predicate (cons other-name other-buffer)) other-name)
       nil
       predicate))))

(defun kubel-switch-to-buffer (buffer-or-name)
  "Display buffer BUFFER-OR-NAME in the selected window.
When called interactively, prompts for a buffer belonging to kubel."
  (interactive (list (kubel--read-buffer)))
  (switch-to-buffer buffer-or-name))

;; popups

(transient-define-prefix kubel-exec-popup ()
  "Kubel Exec Menu"
  ["Actions"
   ("!" "Shell command" kubel-exec-pod-by-shell-command)
   ("d" "Dired" kubel-exec-pod)
   ("e" "Eshell" kubel-exec-eshell-pod)
   ("a" "Ansi-term" kubel-exec-ansi-term-pod)
   ("s" "Shell" kubel-exec-shell-pod)])

(transient-define-prefix kubel-log-popup ()
  "Kubel Log Menu"
  ["Arguments"
   ("-f" "Follow" "-f")
   ("-p" "Previous" "-p")
   ("-n" "Tail" "--tail=")]
  ["Actions"
   ("l" "Tail pod logs" kubel-get-pod-logs)
   ("i" "Tail initContainer logs" kubel-get-pod-logs--initContainer)
   ("L" "Tail by labels" kubel-get-logs-by-labels)])

(transient-define-prefix kubel-copy-popup ()
  "Kubel Copy Menu"
  ["Actions"
   ("c" "Copy resource name" kubel-copy-resource-name)
   ("l" "Copy pod log command" kubel-copy-log-command)
   ("p" "Copy command prefix" kubel-copy-command-prefix)
   ("C" "Copy last command" kubel-copy-last-command)])

(transient-define-prefix kubel-delete-popup ()
  "Kubel Delete menu"
  ["Arguments"
   ("-f" "Force" "--force --grace-period=0")]
  ["Actions"
   ("k" "Delete resource(s)" kubel-delete-resource)])

(transient-define-prefix kubel-describe-popup ()
  "Kubel Describe Menu"
  ["Arguments"
   ("-y" "Yaml" "-o yaml")]
  ["Actions"
   ("RET" "Describe" kubel-get-resource-details)])

(transient-define-prefix kubel-help-popup ()
  "Kubel Menu"
  [["Actions"
    ;; global
    ("RET" "Resource details" kubel-describe-popup)
    ("E" "Quick edit" kubel-quick-edit)
    ("g" "Refresh" kubel-refresh)
    ("b" "Buffers" kubel-switch-to-buffer)
    ("k" "Delete" kubel-delete-popup)
    ("r" "Rollout" kubel-rollout-history)]
   ["" ;; based on current view
    ("p" "Port forward" kubel-port-forward-pod)
    ("l" "Logs" kubel-log-popup)
    ("e" "Exec" kubel-exec-popup)
    ("j" "Jab" kubel-jab-deployment)
    ("S" "Scale replicas" kubel-scale-replicas)]
   ["Settings"
    ("C" "Set context" kubel-set-context)
    ("n" "Set namespace" kubel-set-namespace)
    ("R" "Set resource" kubel-set-resource)
    ("K" "Set kubectl config file" kubel-set-kubectl-config-file)
    ("F" "Set output format" kubel-set-output-format)]
   ["Filter"
    ("f" "Filter" kubel-set-filter)
    ("M-n" "Next highlight" kubel-jump-to-next-highlight)
    ("M-p" "Previous highlight" kubel-jump-to-previous-highlight)
    ("s" "Set label selector" kubel-set-label-selector)]
   ["Marking"
    ("m" "Mark item" kubel-mark-item)
    ("u" "Unmark item" kubel-unmark-item)
    ("M" "Mark all items" kubel-mark-all)
    ("U" "Unmark all items" kubel-unmark-all)]
   ["Utilities"
    ("c" "Copy to clipboad..." kubel-copy-popup)
    ("$" "Show Process buffer" kubel-show-process-buffer)]])

;; mode map
(defvar kubel-mode-map
  (let ((map (make-sparse-keymap)))
    ;; global
    (define-key map (kbd "RET") 'kubel-get-resource-details)
    (define-key map (kbd "K") 'kubel-set-kubectl-config-file)
    (define-key map (kbd "C") 'kubel-set-context)
    (define-key map (kbd "n") 'kubel-set-namespace)
    (define-key map (kbd "g") 'kubel-refresh)
    (define-key map (kbd "h") 'kubel-help-popup)
    (define-key map (kbd "?") 'kubel-help-popup)
    (define-key map (kbd "F") 'kubel-set-output-format)
    (define-key map (kbd "R") 'kubel-set-resource)
    (define-key map (kbd "k") 'kubel-delete-popup)
    (define-key map (kbd "f") 'kubel-set-filter)
    (define-key map (kbd "r") 'kubel-rollout-history)
    (define-key map (kbd "E") 'kubel-quick-edit)
    (define-key map (kbd "M-n") 'kubel-jump-to-next-highlight)
    (define-key map (kbd "M-p") 'kubel-jump-to-previous-highlight)
    (define-key map (kbd "$") 'kubel-show-process-buffer)
    (define-key map (kbd "s") 'kubel-set-label-selector)
    (define-key map (kbd "b") 'kubel-switch-to-buffer)
    ;; based on view
    (define-key map (kbd "p") 'kubel-port-forward-pod)
    (define-key map (kbd "S") 'kubel-scale-replicas)
    (define-key map (kbd "l") 'kubel-log-popup)
    (define-key map (kbd "c") 'kubel-copy-popup)
    (define-key map (kbd "e") 'kubel-exec-popup)
    (define-key map (kbd "!") 'kubel-exec-pod-by-shell-command)
    (define-key map (kbd "j") 'kubel-jab-deployment)

    (define-key map (kbd "m") 'kubel-mark-item)
    (define-key map (kbd "u") 'kubel-unmark-item)
    (define-key map (kbd "M") 'kubel-mark-all)
    (define-key map (kbd "U") 'kubel-unmark-all)

    map)
  "Keymap for `kubel-mode'.")

(defun kubel--current-state ()
  "Show in the Echo Area the current context, namespace, and resource."
  (message (concat
            (format "[Context: %s] [Namespace: %s] [Resource: %s]" kubel-context kubel-namespace kubel-resource)
            (unless (equal kubel-selector "")
              (format " (%s)" kubel-selector)))))

;;;###autoload
(defun kubel-refresh (&optional directory)
  "Refresh the current kubel buffer, calling kubectl using the configured
context, namespace, and resource.

DIRECTORY is optional for TRAMP support."
  (interactive)
  (when directory (setq default-directory directory))
  (let ((name (kubel--buffer-name)))
    ;; Remove old buffer if exist but not is current buffer
    (if (get-buffer name)
        (unless (equal (buffer-name (current-buffer)) name)
          (kill-buffer (get-buffer name))))
    (rename-buffer name)
    (message (format "Running kubectl for: %s..." name)))
  (let ((entries (kubel--populate-list)))
    (setq tabulated-list-format (car entries))
    (setq tabulated-list-entries (cadr entries)))   ; TODO handle "No resource found"
  (setq tabulated-list-sort-key kubel--list-sort-key)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (let ((line-num (line-number-at-pos (point)))
        (current-id (tabulated-list-get-id)))
    (tabulated-list-print t)
    (unless (string-equal current-id (tabulated-list-get-id))
      ;; tabulated-list could not follow the current entry, then fallback on
      ;; keeping the same line.
      (goto-char (point-min))
      (forward-line (1- line-num))))
  (kubel--current-state))

;;;###autoload
(defun kubel-open (context namespace resource &optional directory)
  "Create a new kubel buffer using passed parameters CONTEXT NAMESPACE RESOURCE.
DIRECTORY is optional for TRAMP support."
  (let ((tmpname "*kubel-tmp*")
        (name (kubel--buffer-name-from-parameters context namespace resource)))
    (if (get-buffer name)
        (pop-to-buffer-same-window name)
      (with-current-buffer (get-buffer-create tmpname)
        (kubel-mode)
        (setq kubel-context context)
        (setq kubel-namespace namespace)
        (setq kubel-resource resource)
        (pop-to-buffer-same-window tmpname)
        (kubel-refresh directory)))))

;;;###autoload
(defun kubel (&optional directory)
  "Invoke the kubel buffer.
DIRECTORY is optional for TRAMP support."
  (interactive)
  (let* ((name (kubel--buffer-name))
         (buf (or (get-buffer name)
                  (get-buffer-create name))))
    (with-current-buffer buf
      (switch-to-buffer (current-buffer))
      (unless (eq major-mode 'kubel-mode)
        (kubel-mode))
      (kubel-refresh directory))))

(define-derived-mode kubel-mode tabulated-list-mode "Kubel"
  "Special mode for kubel buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Kubel")
  (setq major-mode 'kubel-mode)
  (use-local-map kubel-mode-map)
  (hl-line-mode 1)
  (run-mode-hooks 'kubel-mode-hook))

(provide 'kubel)
;;; kubel.el ends here
