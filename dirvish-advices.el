;;; dirvish-advices.el --- Shims for other packages. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Shims for other packages.

;;; Code:

(declare-function dirvish-find-file "dirvish")
(declare-function dirvish-dired "dirvish")
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
  '((files         find-file                    dirvish-find-file-ad)
    (dired         dired                        dirvish-dired-ad)
    (dired         dired-other-window           dirvish-dired-ad)
    (dired         dired-other-tab              dirvish-dired-other-tab-ad)
    (dired         dired-other-frame            dirvish-dired-other-frame-ad)
    (dired         dired-jump                   dirvish-dired-jump-ad)
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
    (dired-aux     dired-insert-subdir          dirvish-full-update-frame-ad)
    (dired-aux     dired-do-kill-lines          dirvish-lazy-update-frame-ad)
    (dired-x       dired-omit-mode              dirvish-full-update-frame-ad)
    (dired-narrow  dired--narrow-internal       dirvish-reset-ad)
    (find-dired    find-dired-sentinel          dirvish-reset-ad))
  "A list of FILE, FUNCTION, and ADVICE FUNCTION for `dirvish-override-dired-mode'.")

(defvar dirvish-temporary-advice-alist
  '((isearch       isearch-repeat-backward      dirvish-reset-ad)
    (isearch       isearch-repeat-forward       dirvish-reset-ad)
    (isearch       isearch-exit                 dirvish-reset-ad)
    (evil          evil-refresh-cursor          dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor          dirvish-refresh-cursor-ad)
    (autorevert    doom-auto-revert-buffer-h    ignore) ; For doom-emacs
    (lsp-mode      lsp-deferred                 ignore))
  "A list of FILE, FUNCTION, and ADVICE FUNCTION be temporarily
added in dirvish mode.")

(defun dirvish-switch-buf-other-win-ad (buffer-or-name &optional norecord)
  "Doc."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))
  (let ((pop-up-windows t))
    (pop-to-buffer "*scratch*" t norecord)
    (switch-to-buffer buffer-or-name)))

(defun dirvish-redisplay-frames-fn ()
  "Refresh dirvish frames, added to `after-focus-change-function'."
  (dolist (fr (frame-list))
    (with-selected-frame fr (dirvish--reclaim-current fr))))

(defun dirvish-dired-ad (fn dirname &optional switches)
  "Override `dired' command.
FN refers to original `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when (and (dirvish-curr) (not (dv-one-window-p (dirvish-curr))))
    (dirvish-deactivate))
  (apply fn dirname (and switches (list switches)))
  (dirvish-activate t)
  (when switches
    (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-tab-ad (_ dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab "*scratch*")
  (dirvish-dired dirname)
  (when switches
    (setf (dv-ls-switches (dirvish-curr)) switches)))

(defun dirvish-dired-other-frame-ad (_ dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame "*scratch*")
  (dirvish-dired dirname)
  (when switches
    (setf (dv-ls-switches (dirvish-curr)) switches)))

(defun dirvish-dired-jump-ad (fn &optional other-window file-name)
  "Override `dired-jump' command.
FN refers to original `dired-jump' command.
OTHER-WINDOW and FILE-NAME are same with command `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Dirvish jump to: "))))
  (dirvish--reclaim-current (selected-frame))
  (if (not (dirvish-live-p))
      (apply fn other-window (and file-name (list file-name)))
    (if other-window
        (switch-to-buffer-other-window (dirvish-dired file-name))
      (dirvish-find-file file-name)))
  (dirvish--reclaim-current (selected-frame)))

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

(defun dirvish-find-file-ad (fn &rest args)
  "Apply FN with ARGS with empty `default-directory'."
  (when (dirvish--reclaim-current (selected-frame)) ; reclaim dirvish from minibuffer
    (dirvish-deactivate))
  (let ((default-directory "")) (apply fn args)))

(defun dirvish-update-viewport-h (win _)
  "Refresh attributes in viewport within WIN, added to `window-scroll-functions'."
  (when-let (root-win (and (dirvish-curr) (dv-root-window (dirvish-curr))))
    (when (and (eq win root-win)
               (eq (selected-frame) (window-frame root-win)))
      (with-selected-window win
        (dirvish-body-update nil t)))))

(defun dirvish--add-advices (&optional temporary)
  "Add all advice listed in `dirvish-advice-alist'."
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (when (require file nil t) (advice-add sym :around fn)))
  (when temporary
    (pcase-dolist (`(,file ,sym ,fn) dirvish-temporary-advice-alist)
      (when (require file nil t) (advice-add sym :around fn)))))

(defun dirvish--clean-advices ()
  "Remove all advice listed in `dirvish-advice-alist'."
  (unless (bound-and-true-p dirvish-override-dired-mode)
    (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
      (when (require file nil t) (advice-remove sym fn))))
  (pcase-dolist (`(,file ,sym ,fn) dirvish-temporary-advice-alist)
    (when (require file nil t) (advice-add sym :around fn))))

(provide 'dirvish-advices)

;;; dirvish-advices.el ends here
