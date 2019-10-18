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
;; enter => get pod details
;; h => help popup
;; C => set context
;; n => set namespace
;; g => refresh pods
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

;;; Code:

(define-transient-command kubel-evil-help-popup ()
  "Kubel Evil Menu"
  ["Actions"
   ("ENTER" "Pod details" kubel-get-pod-details)
   ("C" "Set Context" kubel-set-context)
   ("n" "Set namespace" kubel-set-namespace)
   ("g" "Refresh" kubel-mode)
   ("p" "Port forward" kubel-port-forward-pod)
   ("l" "Logs" kubel-log-popup)
   ("c" "Copy" kubel-copy-popup)
   ("o" "Describe" kubel-describe-popup)
   ("e" "Exec" kubel-exec-pod)
   ("d" "Delete" kubel-delete-popup)
   ("a" "Jab" kubel-jab-deployment)])

(evil-set-initial-state 'kubel-mode 'motion)
(evil-define-key 'motion kubel-mode-map
  (kbd "RET") #'kubel-get-pod-details
  (kbd "C") #'kubel-set-context
  (kbd "n") #'kubel-set-namespace
  (kbd "g") #'kubel-mode
  (kbd "p") #'kubel-port-forward-pod
  (kbd "l") #'kubel-log-popup
  (kbd "c") #'kubel-copy-popup
  (kbd "h") #'kubel-evil-help-popup
  (kbd "o") #'kubel-describe-popup
  (kbd "e") #'kubel-exec-pod
  (kbd "d") #'kubel-delete-popup
  (kbd "a") #'kubel-jab-deployment)

(provide 'kubel-evil)

;;; kubel-evil.el ends here
