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
(require 'dirvish-header)
(require 'dirvish-preview)
(require 'dirvish-footer)
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
    (dired         wdired-exit                  dirvish-reset-ad)
    (dired         wdired-finish-edit           dirvish-reset-ad)
    (dired         wdired-abort-changes         dirvish-reset-ad)
    (dired         dired-next-dirline           dirvish-reset-ad)
    (dired-aux     dired-kill-line              dirvish-reset-ad)
    (dired-aux     dired-create-directory       dirvish-reset-ad)
    (dired-aux     dired-create-empty-file      dirvish-reset-ad)
    (dired-aux     dired-do-create-files        dirvish-reset-ad)
    (dired-aux     dired-insert-subdir          dirvish-reset-ad)
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
  (when (memq (previous-frame) dirvish-frame-list)
    (with-selected-frame (previous-frame)
      (when (dirvish-live-p)
        (dirvish-reset t)))))

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

(defun dirvish-refresh-cursor-ad (fn &rest args)
  "Only apply FN with ARGS when editing."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish--update-frame-ad-before ()
  "Helper func for `dirvish-lazy-update-frame-ad'."
  (remove-overlays (point-min) (point-max) 'dirvish-body t)
  (when-let ((pos (dired-move-to-filename nil))
             dirvish-show-icons)
    (remove-overlays (1- pos) pos 'dirvish-icons t)
    (dirvish--body-render-icon pos)))

(defun dirvish--update-frame-ad-after ()
  "Helper func for `dirvish-lazy-update-frame-ad'."
  (when (dired-move-to-filename nil)
    (setf (dirvish-index-path (dirvish-meta)) (dired-get-filename nil t))
    (when (or (dirvish-header-width (dirvish-meta))
              (dirvish-one-window-p (dirvish-meta)))
      (dirvish-header-update))
    (dirvish-footer-update)
    (dirvish-debounce dirvish-preview-update dirvish-preview-delay)))

(defun dirvish-lazy-update-frame-ad (fn &rest args)
  "Apply FN with ARGS then lazily update dirvish frame."
  (dirvish--update-frame-ad-before)
  (apply fn args)
  (dirvish-body-update t t)
  (dirvish--update-frame-ad-after))

(defun dirvish-full-update-frame-ad (fn &rest args)
  "Apply FN with ARGS then fully update dirvish frame."
  (dirvish--update-frame-ad-before)
  (apply fn args)
  (dirvish-body-update)
  (dirvish--update-frame-ad-after))

(defun dirvish-deletion-ad (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (dirvish--get-trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (dirvish-next-file 1))
  (dirvish-reset))

(defun dirvish-file-open-ad (fn &rest args)
  "Apply FN with ARGS with empty `default-directory'."
  (when (dirvish-live-p) (dirvish-quit :keep-alive))
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
  (when-let (meta-info (dirvish-meta))
    (let ((root-win (dirvish-root-window meta-info)))
      (when (and (eq win root-win)
                 (eq (selected-frame) (window-frame root-win)))
        (with-selected-window win
          (dirvish-body-update nil t))))))

(cl-dolist (fn '(dirvish-next-file
                 dirvish-go-top
                 dirvish-go-bottom))
  (advice-add fn :around 'dirvish-lazy-update-frame-ad))

(advice-add #'dirvish-reset :around #'dirvish-full-update-frame-ad)

(defun dirvish--add-advices ()
  "Add all advice listed in `dirvish-advice-alist'."
  (add-hook 'window-scroll-functions #'dirvish-update-viewport-h)
  (add-to-list 'display-buffer-alist
               '("\\(\\*info\\|\\*Help\\|\\*helpful\\|magit:\\).*"
                 (display-buffer-in-side-window)
                 (window-height . 0.4)
                 (side . bottom)))
  (add-function :after after-focus-change-function #'dirvish-redisplay-frames-fn)
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (when (require file nil t) (advice-add sym :around fn))))

(defun dirvish--clean-advices ()
  "Remove all advice listed in `dirvish-advice-alist'."
  (remove-hook 'window-scroll-functions #'dirvish-update-viewport-h)
  (setq display-buffer-alist (cdr display-buffer-alist))
  (remove-function after-focus-change-function #'dirvish-redisplay-frames-fn)
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (when (require file nil t) (advice-remove sym fn))))

(provide 'dirvish-advices)

;;; dirvish-advices.el ends here
