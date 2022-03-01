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

(defcustom dirvish-vc-state-char-alist
  '((up-to-date       . ("  " . vc-up-to-date-state))
    (edited           . ("M " . vc-edited-state))
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

;;;###autoload (autoload 'dirvish--render-vc-gutter-body "dirvish-vc")
;;;###autoload (autoload 'dirvish--render-vc-gutter-line "dirvish-vc")
(dirvish-define-attribute vc-gutter (f-name f-beg hl-face) :lineform
  (when dirvish--vc-backend
    (let* ((state (dirvish-get-attribute-create f-name :vc-gutter nil
                    (let ((f-name (or (file-remote-p f-name 'localname) f-name)))
                      (vc-state-refresh f-name dirvish--vc-backend))))
           (state-cons (alist-get state dirvish-vc-state-char-alist))
           (gutter-str (propertize (car state-cons) 'font-lock-face 'bold))
           (face (cdr state-cons))
           (ov (make-overlay (1- f-beg) f-beg)))
      (if hl-face
          (add-face-text-property 0 (length gutter-str) hl-face t gutter-str)
        (add-face-text-property 0 (length gutter-str) face t gutter-str))
      (overlay-put ov 'dirvish-vc-gutter t)
      (overlay-put ov 'before-string gutter-str))))

;;;###autoload (autoload 'dirvish--render-git-msg-body "dirvish-vc")
;;;###autoload (autoload 'dirvish--render-git-msg-line "dirvish-vc")
(dirvish-define-attribute git-msg (f-name f-end hl-face) :lineform
  (when dirvish--vc-backend
    (let* ((info (dirvish-get-attribute-create f-name :git-msg nil
                   (let* ((f-name (or (file-remote-p f-name 'localname) f-name))
                          (msg (dirvish--shell-to-string "git" "log" "-1" "--pretty=%s" f-name)))
                     (if (and msg (not (string= "" msg))) (substring msg 0 -1) ""))))
           (str (concat "\t" info))
           (ov (make-overlay (1- f-end) f-end)))
      (if hl-face
          (add-face-text-property 0 (length str) hl-face t str)
        (add-face-text-property 0 (length str) 'dirvish-git-commit-message-face t str))
      (overlay-put ov 'dirvish-git-msg t)
      (overlay-put ov 'after-string str))))

;;;###autoload (autoload 'dirvish-vc-diff-preview-dp "dirvish-vc")
(dirvish-define-preview vc-diff ()
  "Show output of `vc-diff' as preview."
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
