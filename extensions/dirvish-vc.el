;;; dirvish-vc.el --- Version-control integration for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Show version-control info such as git commit message at root window and git
;; diff at preview window in Dirvish.

;;; Code:

(declare-function magit-stage-file "magit-apply")
(declare-function magit-unstage-file "magit-apply")
(require 'dirvish)
(define-fringe-bitmap 'dirvish-vc-gutter [250] nil nil '(center repeated))

(defcustom dirvish-vc-state-face-alist
  '((up-to-date       . nil)
    (edited           . vc-edited-state)
    (added            . vc-locally-added-state)
    (removed          . vc-removed-state)
    (missing          . vc-missing-state)
    (needs-merge      . dirvish-vc-needs-merge-face)
    (conflict         . vc-conflict-state)
    (unlocked-changes . vc-locked-state)
    (needs-update     . vc-needs-update-state)
    (ignored          . dired-ignored)
    (unregistered     . dirvish-vc-unregistered-face))
  "Alist of (VC-STATE . FACE).
This value is consumed by `vc-state' attribute in Dirvish.  FACE
is the face used for that VC-STATE.  See `vc-state' in (in
vc-hooks.el) for detail explanation of these states."
  :group 'dirvish
  :type '(alist :key-type symbol :value-type '(symbol :tag "Face")))

(defface dirvish-vc-needs-merge-face
  '((((background dark)) (:background "#500f29"))
    (t                   (:background "#efcbcf")))
  "Face used for `needs-merge' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-unregistered-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for `unregistered' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-git-commit-message-face
  '((t (:inherit font-lock-comment-face)))
  "Face for commit message overlays."
  :group 'dirvish)

(dirvish-define-attribute vc-state
  "The version control state at left fringe."
  (:if (and (eq (dv-root-window dv) (selected-window))
            (dirvish-prop :vc-backend)
            (or (set-window-fringes nil 5 1) t)))
  (let* ((state (dirvish-attribute-cache f-name :vc-state))
         (face (alist-get state dirvish-vc-state-face-alist))
         (display (and face `(left-fringe dirvish-vc-gutter . ,(cons face nil))))
         (gutter-str (and display (propertize "!" 'display display))) ov)
    (when gutter-str
      (prog1 (setq ov (make-overlay f-beg f-beg))
        (overlay-put ov 'before-string gutter-str)))))

(dirvish-define-attribute git-msg
  "Append git commit message to filename."
  (:if (and (eq (dv-root-window dv) (selected-window))
            (eq (dirvish-prop :vc-backend) 'Git)
            (not (dirvish-prop :tramp))))
  (let* ((info (dirvish-attribute-cache f-name :git-msg))
         (face (or hl-face 'dirvish-git-commit-message-face))
         (str (substring (concat "  " info) 0 -1))
         (remain (max (- remain f-wid) 0))
         (len (length str))
         (overflow (< remain len))
         (ov (make-overlay f-end f-end)))
    (and overflow (setq str (substring str 0 remain)))
    (add-face-text-property 0 (if overflow remain len) face t str)
    (overlay-put ov 'after-string str) ov))

(dirvish-define-preview vc-diff ()
  "Show output of `vc-diff' as preview."
  (when (and (dirvish-prop :vc-backend)
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                       ((symbol-function 'message) #'ignore))
               (vc-diff)))
    '(buffer . "*vc-diff*")))

;;;###autoload (autoload 'dirvish-vc-info-ml "dirvish-vc" nil t)
(dirvish-define-mode-line vc-info
  "Version control info such as git branch."
  (when-let* ((bk (dirvish-prop :vc-backend))
              (ml-str (vc-call-backend bk 'mode-line-string default-directory))
              (bk-str (format "%s:" bk)))
    (format " %s %s "
            (propertize bk-str 'face 'bold)
            (propertize (substring ml-str (length bk-str))
                        'face 'font-lock-builtin-face))))

(defun dirvish--magit-on-files (fn &optional fileset)
  "Execute magit function FN to FILESET."
  (unless (featurep 'magit)
    (user-error "Dirvish: install magit.el to use this command"))
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
