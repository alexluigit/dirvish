;;; dirvish-diff.el --- Show diff in Dirvish preview window -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.9.7
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "0.9.9"))

;;; Commentary:

;; This is a dirvish extension which provides commands to show/stage diffs in
;; Dirvish preview window.

;;; Code:

(declare-function magit-stage-file "magit-apply")
(declare-function magit-unstage-file "magit-apply")
(require 'dirvish)

(defvar dirvish-diff-dispatchers
  '(dirvish-preview-disable-dispatcher
    dirvish-preview-directory-exa-dispatcher
    dirvish-vc-diff-dispatcher
    dirvish-preview-default-dispatcher))

(defvar-local dirvish--git-msgs-alist nil)

(defcustom dirvish-git-log-format "%s"
  "Git log pretty format for `dirvish--get-commit-info'.
See PRETTY-FORMAT section of git-log's manpage for details."
  :group 'dirvish :type 'string)

(defcustom dirvish-diff-vc-state-char-alist
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
    (unregistered     . ("  " . vc-state-base))
    (nil              . ("  " . vc-state-base)))
  "Alist of vc-states to indicator characters.
This variable is used in `dirvish--render-gutter'."
  :group 'dirvish
  :type '(alist :key-type symbol :value-type 'cons))

(defface dirvish-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message overlays."
  :group 'dirvish)

(defun dirvish--render-gutter (pos _hl-face)
  "Render vc gutter for file in POS."
  (let* ((entry (dired-get-filename nil 'noerror))
         (state (vc-state entry dirvish--vc-backend))
         (state-cons (alist-get state dirvish-diff-vc-state-char-alist))
         (gutter-str (propertize (car state-cons) 'font-lock-face 'bold))
         (face (cdr state-cons))
         (ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dirvish-diff-gutter t)
    (overlay-put ov 'face face)
    (overlay-put ov 'display gutter-str)))

(defun dirvish--get-commit-info (file)
  "Get commit message info for FILE."
  (let ((file (or (file-remote-p file 'localname) file))
        (msg (alist-get file dirvish--git-msgs-alist nil nil #'string=)))
        (unless msg
          (setq msg (dirvish--shell-to-string
                     "git" "log" "-1" (concat "--pretty=" dirvish-git-log-format)
                     file))
          (when (and msg (not (string= "" msg))) (setq msg (substring msg 0 -1)))
          (push (cons file msg) dirvish--git-msgs-alist))
        msg))

(defun dirvish--render-git-info (_pos hl-face)
  "Render git info with optional HL-FACE."
  (when dirvish--vc-backend
    (dired-move-to-end-of-filename t)
    (let* ((entry (dired-get-filename nil 'noerror))
           (info (dirvish--get-commit-info entry))
           (str (concat "\t" info))
           (ov (make-overlay (1- (point)) (point))))
      (if-let (hl-face (fg (face-attribute hl-face :foreground))
                       (bg (face-attribute hl-face :background)))
          (add-face-text-property 0 (length str) `(:background ,bg :foreground ,fg) t str)
        (add-face-text-property 0 (length str) 'dirvish-commit-message-face t str))
      (overlay-put ov 'dirvish-git-info t)
      (overlay-put ov 'after-string str))))

(defun dirvish-vc-diff-dispatcher (_file _dv)
  "A dispatcher function for `dirvish-diff-dispatchers'.
If `vc-diff' returns t, then show its result buffer as preview."
  (when (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (vc-diff))
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

;;;###autoload
(defun dirvish-diff (&optional path)
  "Open dirvish to show diff generated by version-control tools.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to variable `buffer-file-name'."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish-diff: "))))
  (dirvish-here path
    :depth 0
    :window-conf (current-window-configuration)
    :preview-dispatchers dirvish-diff-dispatchers
    :attributes-alist '((dirvish-diff-gutter . dirvish--render-gutter)
                        (dirvish-git-info . dirvish--render-git-info))))

(provide 'dirvish-diff)
;;; dirvish-diff.el ends here
