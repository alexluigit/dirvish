;;; dirvish-advices.el --- Shims for other packages -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Shims for other packages.

;;; Code:

(declare-function dirvish-mode "dirvish")
(declare-function dirvish-find-file "dirvish")
(declare-function dirvish-body-update "dirvish-body")
(declare-function dirvish--body-render-icon "dirvish-body")
(declare-function dirvish-header-update "dirvish-header")
(declare-function dirvish-footer-update "dirvish-footer")
(declare-function dirvish-preview-update "dirvish-preview")
(require 'cl-lib)
(require 'dirvish-structs)
(require 'dirvish-helpers)
(require 'dirvish-vars)

(defvar dirvish-advice-alist
  '((files         find-file                    dirvish-deactivate-ad          :before)
    (dired         dired-readin                 dirvish-setup-dired-buffer     :after)
    (dired         dired                        dirvish-dired-ad)
    (dired         dired-jump                   dirvish-dired-jump-ad)
    (dired         dired-other-window           dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab              dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame            dirvish-dired-other-frame-ad   :override)
    (dired         dired-next-line              dirvish-dired-next-line-ad     :override)
    (dired         dired-next-dirline           dirvish-lazy-update-ad)
    (dired         dired-mark                   dirvish-lazy-update-ad)
    (dired         dired-flag-file-deletion     dirvish-lazy-update-ad)
    (dired         dired-goto-file              dirvish-lazy-update-ad)
    (dired         dired-internal-do-deletions  dirvish-deletion-ad)
    (wdired        wdired-change-to-wdired-mode dirvish-recover-cursor-ad      :after)
    (wdired        wdired-exit                  dirvish-mode-ad                :after)
    (wdired        wdired-finish-edit           dirvish-mode-ad                :after)
    (wdired        wdired-abort-changes         dirvish-mode-ad                :after)
    (dired-x       dired-omit-mode              dirvish-full-update-ad)
    (dired-aux     dired-dwim-target-next       dirvish-dwim-target-next       :override)
    (dired-aux     dired-insert-subdir          dirvish-full-update-ad)
    (dired-aux     dired-kill-subdir            dirvish-full-update-ad)
    (dired-aux     dired-add-entry              dirvish-lazy-update-ad)
    (dired-aux     dired-remove-entry           dirvish-lazy-update-ad)
    (dired-aux     dired-kill-line              dirvish-lazy-update-ad)
    (dired-aux     dired-do-kill-lines          dirvish-lazy-update-ad)
    (dired-narrow  dired-narrow--internal       dirvish-full-update-ad)
    (dired-subtree dired-subtree-insert         dirvish-full-update-save-pos-ad)
    (dired-subtree dired-subtree-remove         dirvish-full-update-ad))
  "A list of FILE, FUNCTION, and ADVICE FUNCTION used for overriding Dired.")

(defvar dirvish-temporary-advice-alist
  '((evil          evil-refresh-cursor          dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor          dirvish-refresh-cursor-ad)
    (lsp-mode      lsp-deferred                 ignore))
  "A list of FILE, FUNCTION, and temporary ADVICE FUNCTION.
These advices are being added during activation of first dirvish
instance, and get removed when the last dirvish instance exits.")

(defun dirvish-dired-ad (fn dirname &optional switches)
  "Override `dired' command.
FN refers to original `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (and (dirvish-reclaim) (dirvish-deactivate))
  (apply fn dirname (and switches (list switches)))
  (dirvish-activate t)
  (when switches
    (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-next-line-ad (arg)
  "Override `dired-next-line' command.
FN refers to original `dired-next-line' command.
ARG is same with command `dired-next-line'."
  (interactive "^p")
  (dirvish-with-update nil
    (let ((line-move-visual)
	        (goal-column))
      (line-move arg t))
    (while (and (invisible-p (point))
	              (not (if (and arg (< arg 0)) (bobp) (eobp))))
      (forward-char (if (and arg (< arg 0)) -1 1)))
    (and (eobp) (forward-char -1))
    (dired-move-to-filename)))

(defun dirvish-dired-other-window-ad (dirname &optional switches)
  "Override `dired-other-window' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (let ((old-dv (dirvish-curr)))
    (and old-dv (not (dv-one-window-p old-dv)) (dirvish-deactivate))
    (switch-to-buffer-other-window "*scratch*")
    (dirvish-activate t)
    (when switches (setf (dv-ls-switches (dirvish-curr)) switches))
    (dirvish-find-file dirname)))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab "*scratch*")
  (dirvish-drop)
  (dirvish-activate t)
  (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (let (after-focus-change-function)
    (switch-to-buffer-other-frame "*scratch*")
    (dirvish-activate)
    (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
    (dirvish-find-file dirname)))

(defun dirvish-dired-jump-ad (fn &optional other-window file-name)
  "An advisor for `dired-jump' command.
FN refers to original `dired-jump' command.  OTHER-WINDOW and
FILE-NAME are the same args in `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Dirvish jump to: "))))
  (if (and (dirvish-live-p) (not other-window))
      (dirvish-find-file file-name)
    (apply fn other-window (list (or file-name default-directory)))
    (dirvish-setup-dired-buffer)))

(defun dirvish-mode-ad (&rest _)
  "An advisor to enable `dirvish-mode'."
  (dirvish-with-update t (dirvish-mode)))

(defun dirvish-recover-cursor-ad (&rest _)
  "An advisor to recover cursor in current buffer."
  (setq-local cursor-type 'bar))

(defun dirvish-refresh-cursor-ad (fn &rest args)
  "Only apply FN with ARGS when editing filenames in dirvish."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-lazy-update-ad (fn &rest args)
  "Apply FN with ARGS while lazily updating current dirvish."
  (dirvish-with-update nil (apply fn args)))

(defun dirvish-full-update-ad (fn &rest args)
  "Apply FN with ARGS while fully updating current dirvish."
    (dirvish-with-update t (apply fn args)))

(defun dirvish-full-update-save-pos-ad (fn &rest args)
  "Apply FN with ARGS, restore point, then update dirvish frame."
  (dirvish-with-update t (save-excursion (apply fn args))))

(defun dirvish-deletion-ad (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (dirvish--get-trash-dir))) (apply fn args))
  (dirvish-with-update t
    (revert-buffer)
    (unless (dired-get-filename nil t) (dired-next-line 1))))

(defun dirvish-deactivate-ad (&rest _)
  "Quit current dirvish instance if inside one.
Use it as a `:before' advisor to target function."
  (and (dirvish-live-p) (dirvish-deactivate)))

(defun dirvish-setup-dired-buffer (&rest _)
  "Setup Dired buffer for dirvish.
This function removes the header line in a Dired buffer."
  (save-excursion
    (let ((o (make-overlay
              (point-min)
              (progn (goto-char (point-min)) (forward-line 1) (point)))))
      (overlay-put o 'invisible t))))

(defun dirvish-update-viewport-h (win _)
  "Refresh attributes in viewport within WIN, added to `window-scroll-functions'."
  (let ((buf (current-buffer)))
    (when (and (dirvish-live-p win)
               ;; Do not update when current buffer exists in multiple windows
               (< (cl-count-if (lambda (w) (eq (window-buffer w) buf)) (window-list)) 2))
      (dirvish-body-update nil t))))

(defun dirvish--add-advices (&optional temporary)
  "Add all advice listed in `dirvish-advice-alist'.
When TEMPORARY is non-nil, also add advices in
`dirvish-temporary-advice-alist'."
  (pcase-dolist (`(,file ,sym ,fn ,place) dirvish-advice-alist)
    (when (require file nil t) (advice-add sym (or place :around) fn)))
  (when temporary
    (add-hook 'window-scroll-functions #'dirvish-update-viewport-h)
    (add-hook 'window-selection-change-functions #'dirvish-reclaim)
    (pcase-dolist (`(,file ,sym ,fn ,place) dirvish-temporary-advice-alist)
      (when (require file nil t) (advice-add sym (or place :around) fn)))))

(defun dirvish--clean-advices ()
  "Remove most of advices added by dirvish.
This function does not remove advices added by
`dirvish-override-dired-mode'."
  (unless (bound-and-true-p dirvish-override-dired-mode)
    (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
      (when (require file nil t) (advice-remove sym fn))))
  (pcase-dolist (`(,file ,sym ,fn) dirvish-temporary-advice-alist)
    (when (require file nil t) (advice-remove sym fn)))
  (remove-hook 'window-scroll-functions #'dirvish-update-viewport-h)
  (remove-hook 'window-selection-change-functions #'dirvish-reclaim))

(provide 'dirvish-advices)

;;; dirvish-advices.el ends here
