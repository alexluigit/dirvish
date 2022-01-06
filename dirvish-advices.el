;;; dirvish-advices.el --- Shims for other packages -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Shims for other packages.

;;; Code:

(declare-function dirvish-find-file "dirvish")
(declare-function dirvish-dired "dirvish")
(declare-function dirvish-quit "dirvish")
(declare-function dirvish-reset "dirvish")
(declare-function dirvish-header-update "dirvish-header")
(declare-function dirvish-footer-update "dirvish-footer")
(declare-function dirvish-preview-update "dirvish-preview")
(require 'cl-lib)
(require 'dirvish-structs)
(require 'dirvish-helpers)
(require 'dirvish-body)
(require 'dirvish-vars)

(defvar dirvish-advice-alist
  '((files         find-file                    dirvish-deactivate-ad          :before)
    (dired         dired-readin                 dirvish-setup-dired-buffer     :after)
    (dired         dired                        dirvish-dired-ad)
    (dired         dired-other-window           dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab              dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame            dirvish-dired-other-frame-ad   :override)
    (dired         dired-jump                   dirvish-dired-jump-ad)
    (dired         dired-next-line              dirvish-dired-next-line-ad     :override)
    (dired         dired-mark                   dirvish-lazy-update-frame-ad)
    (dired         dired-flag-file-deletion     dirvish-lazy-update-frame-ad)
    (dired         dired-goto-file              dirvish-lazy-update-frame-ad)
    (dired         dired-internal-do-deletions  dirvish-deletion-ad)
    (dired         dired-next-dirline           dirvish-reset-ad)
    (wdired        wdired-change-to-wdired-mode dirvish-recover-cursor-ad)
    (wdired        wdired-exit                  dirvish-reset-ad)
    (wdired        wdired-finish-edit           dirvish-reset-ad)
    (wdired        wdired-abort-changes         dirvish-reset-ad)
    (dired-aux     dired-kill-line              dirvish-reset-ad)
    (dired-aux     dired-create-directory       dirvish-reset-ad)
    (dired-aux     dired-create-empty-file      dirvish-reset-ad)
    (dired-aux     dired-do-create-files        dirvish-reset-ad)
    (dired-aux     dired-kill-subdir            dirvish-reset-ad)
    (dired-aux     dired-insert-subdir          dirvish-full-update-frame-ad)
    (dired-aux     dired-do-kill-lines          dirvish-lazy-update-frame-ad)
    (dired-x       dired-omit-mode              dirvish-full-update-frame-ad)
    (dired-narrow  dired--narrow-internal       dirvish-reset-ad)
    (find-dired    find-dired-sentinel          dirvish-reset-ad))
  "A list of FILE, FUNCTION, and ADVICE FUNCTION used for overriding Dired.")

(defvar dirvish-temporary-advice-alist
  '((isearch       isearch-repeat-backward      dirvish-reset-ad)
    (isearch       isearch-repeat-forward       dirvish-reset-ad)
    (isearch       isearch-exit                 dirvish-reset-ad)
    (evil          evil-refresh-cursor          dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor          dirvish-refresh-cursor-ad)
    (autorevert    doom-auto-revert-buffer-h    ignore) ; For doom-emacs
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

(defun dirvish-reset-ad (fn &rest args)
  "Apply FN with ARGS, rebuild dirvish frame when necessary."
  (apply fn args)
  (let ((rebuild (not (eq major-mode 'dirvish-mode))))
    (dirvish-reset rebuild 'no-revert)))

(defun dirvish-recover-cursor-ad (fn &rest args)
  "Apply FN with ARGS then show cursor."
  (apply fn args)
  (setq-local cursor-type 'bar))

(defun dirvish-refresh-cursor-ad (fn &rest args)
  "Only apply FN with ARGS when editing filenames in dirvish."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-lazy-update-frame-ad (fn &rest args)
  "Apply FN with ARGS while lazily updating dirvish frame."
  (dirvish-with-update nil (apply fn args)))

(defun dirvish-full-update-frame-ad (fn &rest args)
  "Apply FN with ARGS while fully updating dirvish frame."
  (dirvish-with-update t (apply fn args)))

(defun dirvish-deletion-ad (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (dirvish--get-trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (dirvish-next-file 1))
  (dirvish-reset))

(defun dirvish-deactivate-ad (&rest _)
  "Quit current dirvish instance if inside one.
Use it as a `:before' advisor to target function."
  (dirvish-deactivate))

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
