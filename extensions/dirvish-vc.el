;;; dirvish-vc.el --- Version-control integration for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.2.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.2.0"))

;;; Commentary:

;; Show version-control info such as git commit message at root window and git
;; diff at preview window in Dirvish.

;;; Code:

(declare-function magit-stage-file "magit-apply")
(declare-function magit-unstage-file "magit-apply")
(require 'dirvish)

(defcustom dirvish-vc-state-char-alist
  '((up-to-date       . (" " . vc-up-to-date-state))
    (edited           . ("M" . vc-edited-state))
    (added            . ("+" . vc-locally-added-state))
    (removed          . ("-" . vc-removed-state))
    (missing          . ("!" . vc-missing-state))
    (needs-merge      . ("M" . vc-state-base))
    (conflict         . ("!" . vc-conflict-state))
    (unlocked-changes . ("!" . vc-locked-state))
    (needs-update     . ("U" . vc-needs-update-state))
    (ignored          . (" " . dired-ignored))
    (user             . ("U" . vc-state-base))
    (unregistered     . ("?" . vc-state-base))
    (nil              . (" " . vc-state-base)))
  "Alist of vc-states to indicator characters.
This variable is consumed by `vc-state' attribute in Dirvish."
  :group 'dirvish
  :type '(alist :key-type symbol :value-type 'cons))

(defface dirvish-git-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message overlays."
  :group 'dirvish)

(dirvish-define-attribute vc-state
  :if (and (eq (dv-root-window dv) (selected-window)) dirvish--vc-backend)
  :left 1
  :form
  (let* ((state (dirvish-attribute-cache f-name :vc-state
                  (let ((f-name (or (file-remote-p f-name 'localname) f-name)))
                    (vc-state-refresh f-name dirvish--vc-backend))))
         (state-cons (alist-get state dirvish-vc-state-char-alist))
         (gutter-str (propertize (car state-cons) 'font-lock-face 'bold))
         (face (or hl-face (cdr state-cons)))
         (ov (make-overlay l-beg l-beg)))
    (add-face-text-property 0 (length gutter-str) face t gutter-str)
    (overlay-put ov 'before-string gutter-str) ov))

(dirvish-define-attribute git-msg
  :if (and (eq (dv-root-window dv) (selected-window)) dirvish--vc-backend)
  :form
  (let* ((info (dirvish-attribute-cache f-name :git-msg
                 (let* ((f-name (or (file-remote-p f-name 'localname) f-name))
                        (msg (dirvish--shell-to-string "git" "log" "-1" "--pretty=%s" f-name)))
                   (if (and msg (not (string= "" msg))) (substring msg 0 -1) ""))))
         (face (or hl-face 'dirvish-git-commit-message-face))
         (width (window-width))
         (depth (* dirvish--subtree-prefix-len (dirvish--get-subtree-depth)))
         (f-base-str (buffer-substring f-beg f-end))
         (f-base-len (dirvish--actual-string-length f-base-str))
         (remained (- width f-base-len depth (car dirvish--attrs-width) (cdr dirvish--attrs-width)))
         (msg-str (truncate-string-to-width (concat "\t" info) remained))
         (ov (make-overlay (1- f-end) f-end)))
    (add-face-text-property 0 (length msg-str) face t msg-str)
    (overlay-put ov 'after-string msg-str) ov))

;;;###autoload (autoload 'dirvish-vc-diff-preview-dp "dirvish-vc")
(dirvish-define-preview vc-diff ()
  "Show output of `vc-diff' as preview."
  (when (and dirvish--vc-backend
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                       ((symbol-function 'message) #'ignore))
               (vc-diff)))
    '(buffer . "*vc-diff*")))

;;;###autoload (autoload 'dirvish-vc-info-ml "dirvish-vc")
(dirvish-define-mode-line vc-info
  (when dirvish--vc-backend
    (let ((ml-str (vc-call-backend
                   dirvish--vc-backend
                   'mode-line-string default-directory))
          (backend-str (format "%s:" dirvish--vc-backend)))
      (format " %s %s "
              (propertize backend-str 'face 'bold)
              (propertize (substring ml-str (length backend-str))
                          'face 'font-lock-builtin-face)))))

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
