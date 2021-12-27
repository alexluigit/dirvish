;;; dirvish-advices.el --- Shims for other packages. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Shims for other packages.

;;; Code:

(declare-function dirvish-quit "dirvish")
(declare-function dirvish-reset "dirvish")
(declare-function dirvish-new-frame "dirvish")
(declare-function dirvish-next-file "dirvish")
(require 'cl-lib)
(require 'dirvish-structs)
(require 'dirvish-helpers)
(require 'dirvish-body)
(require 'dirvish-vars)

(defvar dirvish-advice-alist
  '((files         find-file                    dirvish-file-open-ad)
    (files         find-file-other-window       dirvish-file-open-ad)
    (dired         dired-find-file-other-window dirvish-other-window-ad)
    (dired         dired-readin                 dirvish-setup-dired-buffer-ad)
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
    (dired-aux     dired-do-kill-lines          dirvish-lazy-update-frame-ad)
    (dired-x       dired-omit-mode              dirvish-full-update-frame-ad)
    (dired-narrow  dired--narrow-internal       dirvish-reset-ad)
    (isearch       isearch-repeat-backward      dirvish-reset-ad)
    (isearch       isearch-repeat-forward       dirvish-reset-ad)
    (isearch       isearch-exit                 dirvish-reset-ad)
    (find-dired    find-dired-sentinel          dirvish-reset-ad)
    (evil          evil-refresh-cursor          dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor          dirvish-refresh-cursor-ad)
    (autorevert    doom-auto-revert-buffer-h    ignore) ; For doom-emacs
    (lsp-mode      lsp-deferred                 ignore))
  "A list of file, adviced function, and advice function.

This variable is consumed by `dirvish--add-advices'.")

(defun dirvish-redisplay-frames-fn ()
  "Refresh dirvish frames, added to `after-focus-change-function'."
  (when (eq major-mode 'dirvish-mode)
    (dirvish-reset t))
  (with-selected-frame (previous-frame)
    (dirvish--reclaim-current (previous-frame))))

(defun dirvish-setup-dired-buffer-ad (fn &rest args)
  "Apply FN with ARGS, remove the header line in Dired buffer."
  (apply fn args)
  (save-excursion
    (let ((o (make-overlay (point-min) (progn (forward-line 1) (point)))))
      (overlay-put o 'invisible t))))

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

(defun dirvish-file-open-ad (fn &rest args)
  "Apply FN with ARGS with empty `default-directory'."
  (when (dirvish-live-p) (dirvish-quit :keep-frame))
  (let ((default-directory "")) (apply fn args)))

;; FIXME: it should support window when current instance is launched by `(dirvish nil t)'
(defun dirvish-other-window-ad (fn &rest args)
  "Apply FN with ARGS in new dirvish frame."
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dirvish-new-frame file)
      (apply fn args))))

(defun dirvish-update-viewport-h (win _)
  "Refresh attributes in viewport within WIN, added to `window-scroll-functions'."
  (when-let (root-win (and (dirvish-curr) (dv-root-window (dirvish-curr))))
    (when (and (eq win root-win)
               (eq (selected-frame) (window-frame root-win)))
      (with-selected-window win
        (dirvish-body-update nil t)))))

(defun dirvish--add-advices ()
  "Add all advice listed in `dirvish-advice-alist'."
  (add-to-list 'display-buffer-alist
               '("\\(\\*info\\|\\*Help\\|\\*helpful\\|magit:\\).*"
                 (display-buffer-in-side-window)
                 (window-height . 0.4)
                 (side . bottom)))
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (when (require file nil t) (advice-add sym :around fn))))

(defun dirvish--clean-advices ()
  "Remove all advice listed in `dirvish-advice-alist'."
  (setq display-buffer-alist (cdr display-buffer-alist))
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (when (require file nil t) (advice-remove sym fn))))

(provide 'dirvish-advices)

;;; dirvish-advices.el ends here
