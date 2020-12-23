;;; kubel-evil.el --- extension for kubel to provide evil keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Marcel Patzwahl

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
;; Author: Marcel Patzwahl
;; Keywords: kubernetes k8s tools processes evil keybindings
;; URL: https://github.com/abrochard/kubel
;; License: GNU General Public License >= 3
;; Package-Requires: ((kubel "1.0") (evil "1.0") (emacs "25.3"))

;;; Commentary:

;; Emacs extension for controlling Kubel with evil keybindings.

;;; Shortcuts:

;; On the kubel screen, place your cursor on the pod
;;
;; enter => get resource details
;; h => help popup
;; C => set context
;; n => set namespace
;; R => set resource
;; g => refresh pods
;; E => quick edit
;; p => port forward pod
;; e => exec into pod
;; o => describe popup
;; l => log popup
;; c => copy popup
;; d => delete pod
;; a => jab deployment to force rolling update
;;

;;; Customize:
(require 'evil)
(require 'kubel)

(defgroup kubel-evil nil
  "Provides integration of kubel and evil."
  :group 'kubel
  :prefix "kubel-evil-")

(defvar kubel-evil-mode-map (make-sparse-keymap))

;;; Code:
(define-minor-mode kubel-evil-mode
  "Brings evil keybindings to kubel"
  :lighter " kubel-evil"
  :keymap kubel-evil-mode-map
  :group 'kubel-evil)

(add-hook 'kubel-mode-hook 'kubel-evil-mode)

(define-transient-command kubel-evil-help-popup ()
  "Kubel Evil Menu"
  [["Actions"
    ;; global
    ("RET" "Resource details" kubel-describe-popup)
    ("E" "Quick edit" kubel-quick-edit)
    ("g" "Refresh" kubel)
    ("d" "Delete" kubel-delete-popup) ;; can't use k here
    ("r" "Rollout" kubel-rollout-history)]
   ["" ;; based on current view
    ("p" "Port forward" kubel-port-forward-pod)
    ("l" "Logs" kubel-log-popup)
    ("e" "Exec" kubel-exec-popup)
    ("a" "Jab" kubel-jab-deployment)] ;; can't use j here
   ["Settings"
    ("C" "Set context" kubel-set-context)
    ("n" "Set namespace" kubel-set-namespace)
    ("R" "Set resource" kubel-set-resource)
    ("K" "Set kubectl config file" kubel-set-kubectl-config-file)
    ("F" "Set output format" kubel-set-output-format)]
   ["Filter"
    ("f" "Filter" kubel-set-filter)
    ("M-n" "Next highlight" kubel-jump-to-next-highlight)
    ("M-p" "Previous highlight" kubel-jump-to-previous-highlight)]
   ["Marking"
    ("m" "Mark item" kubel-mark-item)
    ("u" "Unmark item" kubel-unmark-item)
    ("M" "Mark all items" kubel-mark-all)
    ("U" "Unmark all items" kubel-unmark-all)]
   ["Utilities"
    ("c" "Copy to clipboad..." kubel-copy-popup)
    ("$" "Show Process buffer" kubel-show-process-buffer)]])

(evil-set-initial-state 'kubel-mode 'motion)

(evil-define-key 'motion kubel-evil-mode-map
  (kbd "RET") #'kubel-get-resource-details
  (kbd "K") #'kubel-set-kubectl-config-file
  (kbd "C") #'kubel-set-context
  (kbd "n") #'kubel-set-namespace
  (kbd "g") #'kubel
  (kbd "h") #'kubel-evil-help-popup
  (kbd "?") #'kubel-evil-help-popup
  (kbd "F") #'kubel-set-output-format
  (kbd "R") #'kubel-set-resource
  (kbd "d") #'kubel-delete-popup
  (kbd "f") #'kubel-set-filter
  (kbd "r") #'kubel-rollout-history
  (kbd "E") #'kubel-quick-edit
  (kbd "M-n") #'kubel-jump-to-next-highlight
  (kbd "M-p") #'kubel-jump-to-previous-highlight
  (kbd "$") #'kubel-show-process-buffer
  (kbd "p") #'kubel-port-forward-pod
  (kbd "l") #'kubel-log-popup
  (kbd "c") #'kubel-copy-popup
  (kbd "e") #'kubel-exec-popup
  (kbd "a") #'kubel-jab-deployment
  (kbd "u") #'kubel-unmark-item
  (kbd "M") #'kubel-mark-all
  (kbd "U") #'kubel-unmark-all)

(provide 'kubel-evil)

;;; kubel-evil.el ends here
