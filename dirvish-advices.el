;;; dirvish-advices.el --- Accommodations for other packages -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Accommodations for other packages.

;;; Code:

(declare-function dirvish-dired "dirvish")
(defvar fd-dired-buffer-name-format)
(require 'dirvish-commands)

(defun dirvish-subtree-remove-ad (fn &rest _)
  "Advisor for FN `dired-subtree-remove'."
  (dirvish--hide-dired-header (funcall fn))) ; See `dired-hacks' #170

(defun dirvish-dired-ad (fn dirname &optional switches)
  "Override `dired' command.
FN refers to original `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when-let ((dv (dirvish-curr))) (dirvish-deactivate dv))
  (apply fn dirname (and switches (list switches)))
  (dirvish-activate (dirvish-new :depth -1))
  (when switches
    (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-window-ad (dirname &optional switches)
  "Override `dired-other-window' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when-let ((dv (dirvish-curr)))
    (unless (dirvish-dired-p dv) (dirvish-deactivate dv)))
  (switch-to-buffer-other-window (dirvish--ensure-temp-buffer))
  (dirvish-activate (dirvish-new :depth -1))
  (when switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab (dirvish--ensure-temp-buffer))
  (dirvish-drop)
  (dirvish-activate (dirvish-new :depth -1))
  (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (let (after-focus-change-function)
    (switch-to-buffer-other-frame (dirvish--ensure-temp-buffer))
    (dirvish-activate (dirvish-new :depth dirvish-depth))
    (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
    (dirvish-find-file dirname)))

(defun dirvish-dired-jump-ad (_fn &optional other-window file-name)
  "An advisor for `dired-jump' command.
OTHER-WINDOW and FILE-NAME are the same args in `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg (read-file-name "Dirvish jump to: "))))
  (if (and (dirvish-curr) (not other-window))
      (dirvish-find-file file-name)
    (dirvish-dired (or file-name default-directory) other-window)))

(defun dirvish-find-dired-sentinel-ad (&rest _)
  "Advisor function for `find-dired-sentinel'."
  (let ((dv (dirvish-curr))
        (last-dv (with-current-buffer (other-buffer)
                   (when (derived-mode-p 'dirvish-mode) (dirvish-curr))))
        buffer-read-only)
    (unless (and dv (eq (dv-type dv) 'find-dired))
      (let* ((last-depth
              (with-current-buffer (other-buffer)
                (and (derived-mode-p 'dirvish-mode) (dv-depth (dirvish-curr)))))
             (new-dv (dirvish-new :type 'find-dired :dedicated t)))
        (add-to-list 'dirvish--transient-dvs new-dv)
        (setf (dv-transient new-dv) (or last-dv new-dv))
        (dirvish-activate new-dv)
        (setf (dv-depth new-dv) (or last-depth 0))
        (setq-local dirvish--curr-name (dv-name new-dv))
        (dirvish-reclaim)
        (dirvish-build)))
    ;; BUG?: `dired-move-to-filename' failed to parse filename when there is only 1 file in buffer
    (delete-matching-lines "find finished at.*\\|^ +$")
    (dirvish--hide-dired-header)
    (and (dirvish-curr) (dirvish-setup 'keep-dired))))

(defun dirvish-fd-dired-ad (fn &rest args)
  "Advisor function for FN `fd-dired' with its ARGS."
  (when (and (dirvish-curr) (eq (dv-type (dirvish-curr)) 'find-dired))
    (dirvish-deactivate (dirvish-curr)))
  ;; HACK for *FD* window placement. `fd-dired-display-in-current-window' does not behave as described.
  (let ((display-buffer-alist '(("^ ?\\*Fd.*$" (display-buffer-same-window))))
        (fd-dired-buffer-name-format "*%s*"))
    (apply fn args)))

(defun dirvish-dwim-target-next-ad (&optional all-frames)
  "Replacement for `dired-dwim-target-next'.
If ALL-FRAMES, search target directories in all frames."
  (mapcan (lambda (w)
            (when (or all-frames
                      (eq (and (window-valid-p w) (window-frame w)) (selected-frame)))
              (with-current-buffer (window-buffer w)
                (list (dired-current-directory)))))
          (delq (selected-window) (dirvish-get-all 'root-window t t))))

(defun dirvish-wdired-mode-ad (&rest _)
  "Advisor function for `wdired-change-to-wdired-mode'."
  (dired-move-to-end-of-filename t)
  (setq-local cursor-type '(bar 4))
  (dolist (ov (mapcar #'car (dv-attributes-alist (dirvish-curr))))
    (remove-overlays (point-min) (point-max) ov t))
  (remove-hook 'post-command-hook #'dirvish-update-body-h :local))

(defun dirvish-refresh-cursor-ad (fn &rest args)
  "Only apply FN with ARGS when editing filenames in dirvish."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-deletion-ad (fn &rest args)
  "Advice function for FN `dired-internal-do-deletions' with its ARGS."
  (let ((trash-directory (dirvish--get-trash-dir))) (apply fn args)))

(defun dirvish-find-file-ad (&rest _)
  "Quit Dirvish session if inside one."
  (when-let* ((dv (dirvish-curr)))
    (if-let ((transient (dv-transient dv)))
        (dirvish--end-transient transient)
      (select-window (funcall (dv-find-file-window-fn dv)))
      (when (dirvish-live-p dv) (dirvish-deactivate dv)))))

(defun dirvish-ignore-ad (fn &rest args)
  "Only apply FN with ARGS outside of Dirvish."
  (unless (dirvish-curr) (apply fn args)))

(provide 'dirvish-advices)
;;; dirvish-advices.el ends here
