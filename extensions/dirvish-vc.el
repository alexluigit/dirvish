;;; dirvish-vc.el --- Version-control integration for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.9.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "0.9.9"))

;;; Commentary:

;; Show version-control info such as git commit message at root window and git
;; diff at preview window in Dirvish.

;;; Code:

(declare-function magit-stage-file "magit-apply")
(declare-function magit-unstage-file "magit-apply")
(require 'dirvish)

(defvar-local dirvish--git-msgs-alist nil)
(defvar-local dirvish--vc-state-alist nil)

(defcustom dirvish-vc-state-char-alist
  '((up-to-date       . ("  " . vc-up-to-date-state))
    (edited           . ("E " . vc-edited-state))
    (added            . ("+ " . vc-locally-added-state))
    (removed          . ("- " . vc-removed-state))
    (missing          . ("! " . vc-missing-state))
    (needs-merge      . ("M " . vc-state-base))
    (conflict         . ("! " . vc-conflict-state))
    (unlocked-changes . ("! " . vc-locked-state))
    (needs-update     . ("U " . vc-needs-update-state))
    (ignored          . ("  " . vc-dir-status-ignored))
    (user             . ("U " . vc-state-base))
    (unregistered     . ("? " . vc-state-base))
    (nil              . ("  " . vc-state-base)))
  "Alist of vc-states to indicator characters.
This variable is used in `dirvish--render-gutter'."
  :group 'dirvish
  :type '(alist :key-type symbol :value-type 'cons))

(defface dirvish-git-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message overlays."
  :group 'dirvish)

(defun dirvish--get-vc-state (file backend)
  "Get vc state for FILE with BACKEND."
  (let ((file (or (file-remote-p file 'localname) file))
        (state (alist-get file dirvish--vc-state-alist nil nil #'string=)))
    (unless state
      (setq state (vc-state-refresh file backend))
      (push (cons file state) dirvish--vc-state-alist))
    state))

;;;###autoload
(defun dirvish--render-vc-gutter (pos _hl-face)
  "Render vc gutter for file in POS."
  (when dirvish--vc-backend
    (let* ((entry (dired-get-filename nil 'noerror))
           (state (dirvish--get-vc-state entry dirvish--vc-backend))
           (state-cons (alist-get state dirvish-vc-state-char-alist))
           (gutter-str (propertize (car state-cons) 'font-lock-face 'bold))
           (face (cdr state-cons))
           (ov (make-overlay (1- pos) pos)))
      (overlay-put ov 'dirvish-vc-gutter t)
      (overlay-put ov 'face face)
      (overlay-put ov 'display gutter-str))))

(defun dirvish--get-git-commit-msg (file)
  "Get commit message info for FILE."
  (let ((file (or (file-remote-p file 'localname) file))
        (msg (alist-get file dirvish--git-msgs-alist nil nil #'string=)))
    (unless msg
      (setq msg (dirvish--shell-to-string "git" "log" "-1" "--pretty=%s" file))
      (when (and msg (not (string= "" msg))) (setq msg (substring msg 0 -1)))
      (push (cons file msg) dirvish--git-msgs-alist))
    msg))

;;;###autoload
(defun dirvish--render-git-msg (_pos hl-face)
  "Render git info with optional HL-FACE."
  (when dirvish--vc-backend
    (dired-move-to-end-of-filename t)
    (let* ((entry (dired-get-filename nil 'noerror))
           (info (dirvish--get-git-commit-msg entry))
           (str (concat "\t" info))
           (ov (make-overlay (1- (point)) (point))))
      (if-let (hl-face (fg (face-attribute hl-face :foreground))
                       (bg (face-attribute hl-face :background)))
          (add-face-text-property 0 (length str) `(:background ,bg :foreground ,fg) t str)
        (add-face-text-property 0 (length str) 'dirvish-git-commit-message-face t str))
      (overlay-put ov 'dirvish-git-msg t)
      (overlay-put ov 'after-string str))))

;;;###autoload
(defun dirvish-preview-vc-diff-dispatcher (_file _dv)
  "A dispatcher function for `dirvish-preview-dispatchers'.
If `vc-diff' returns t, then show its result buffer as preview."
  (when (and dirvish--vc-backend
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                       ((symbol-function 'message) #'ignore))
               (vc-diff)))
    '(buffer . "*vc-diff*")))

(defun dirvish--magit-on-files (fn &optional fileset)
  "Execute magit function FN to FILESET."
  (unless (featurep 'magit) (user-error "Dirvish: install magit.el to use this command"))
  (setq fileset (or fileset (dired-get-marked-files)))
  (cl-dolist (file fileset) (funcall fn file))
  (dired-unmark-all-marks)
  (revert-buffer))

(defun dirvish-magit-stage-files (&optional fileset)
  "Stage vc diffs of FILESET using `magit-stage-file'."
  (interactive)
  (dirvish--magit-on-files #'magit-stage-file fileset))

(defun dirvish-magit-unstage-files (&optional fileset)
  "Unstage vc diffs of FILESET using `magit-unstage-file'."
  (interactive)
  (dirvish--magit-on-files #'magit-unstage-file fileset))

(provide 'dirvish-vc)
;;; dirvish-vc.el ends here
