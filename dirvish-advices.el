;;; dirvish-advices.el --- Accommodations for other packages -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Accommodations for other packages.

;;; Code:

(defalias 'dirvish-enlarge-ad #'dirvish--enlarge)
(defvar fd-dired-buffer-name-format)
(require 'dirvish-core)
(require 'dirvish-commands)
(require 'dirvish-helpers)
(require 'dirvish-options)

(defvar dirvish-advice-alist
  '((files         find-file                       dirvish-find-file-ad           :before)
    (dired         dired                           dirvish-dired-ad)
    (dired         dired-jump                      dirvish-dired-jump-ad)
    (dired         dired-find-file                 dirvish-find-file              :override)
    (dired         dired-find-alternate-file       dirvish-find-file              :override)
    (dired         dired-other-window              dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab                 dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame               dirvish-dired-other-frame-ad   :override)
    (dired         dired-up-directory              dirvish-up-directory           :override)
    (dired         dired-sort-toggle-or-edit       dirvish-sort-by-criteria       :override)
    (dired         +dired/quit-all                 quit-window                    :override)
    (dired         dired-view-file                 dirvish-enlarge-ad             :before)
    (dired         dired-internal-do-deletions     dirvish-deletion-ad)
    (wdired        wdired-change-to-wdired-mode    dirvish-wdired-mode-ad         :after)
    (wdired        wdired-exit                     dirvish-setup                  :after)
    (wdired        wdired-finish-edit              dirvish-setup                  :after)
    (wdired        wdired-abort-changes            dirvish-setup                  :after)
    (find-dired    find-dired-sentinel             dirvish-find-dired-sentinel-ad :after)
    (fd-dired      fd-dired                        dirvish-fd-dired-ad)
    (dired-aux     dired-dwim-target-next          dirvish-dwim-target-next-ad    :override)
    (evil          evil-refresh-cursor             dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor             dirvish-refresh-cursor-ad)
    (magit         magit-status-setup-buffer       dirvish-enlarge-ad             :before)
    (winner        winner-save-old-configurations  dirvish-ignore-ad)
    (lsp-mode      lsp-deferred                    dirvish-ignore-ad)
    (recentf       recentf-track-opened-file       dirvish-ignore-ad)
    (recentf       recentf-track-closed-file       dirvish-ignore-ad))
  "A list of FILE, FUNCTION, and ADVICE FUNCTION used for overriding Dired.")

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
  (switch-to-buffer-other-window dirvish-temp-buffer)
  (dirvish-activate (dirvish-new :depth -1))
  (when switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab dirvish-temp-buffer)
  (dirvish-drop)
  (dirvish-activate (dirvish-new :depth -1))
  (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (let (after-focus-change-function)
    (switch-to-buffer-other-frame dirvish-temp-buffer)
    (dirvish-activate (dirvish-new :depth dirvish-depth))
    (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
    (dirvish-find-file dirname)))

(defun dirvish-dired-jump-ad (fn &optional other-window file-name)
  "An advisor for `dired-jump' command.
FN refers to original `dired-jump' command.  OTHER-WINDOW and
FILE-NAME are the same args in `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg
                  (read-file-name "Dirvish jump to: "))))
  (if (and (dirvish-curr) (not other-window))
      (dirvish-find-file file-name)
    (apply fn other-window (list (or file-name default-directory)))
    (dirvish-setup-dired-buffer)))

(defun dirvish-find-dired-sentinel-ad (&rest _)
  "Advisor function for `find-dired-sentinel'."
  (let ((pt-min (point-min))
        (dv (dirvish-curr))
        (last-dv (with-current-buffer (other-buffer)
                   (when (derived-mode-p 'dirvish-mode) (dirvish-curr))))
        buffer-read-only)
    (unless (and dv (eq (dv-type dv) 'find-dired))
      (let* ((last-depth
              (with-current-buffer (other-buffer)
                (and (derived-mode-p 'dirvish-mode) (dv-depth (dirvish-curr)))))
             (new-dv (dirvish-new :type 'find-dired :dedicated t)))
        (add-to-list 'dirvish-transient-dvs new-dv)
        (setf (dv-transient new-dv) (or last-dv new-dv))
        (dirvish-activate new-dv)
        (setf (dv-depth new-dv) (or last-depth 0))
        (setq-local dirvish--curr-name (dv-name new-dv))
        (dirvish-reclaim)
        (dirvish-build)))
    ;; BUG?: `dired-move-to-filename' failed to parse filename when there is only 1 file in buffer
    (delete-matching-lines "find finished at.*\\|^ +$")
    (delete-region pt-min (progn (goto-char pt-min) (forward-line 2) (point)))
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
            (when (or all-frames (eq (window-frame w) (selected-frame)))
              (with-current-buffer (window-buffer w)
                (list (dired-current-directory)))))
          (delq (selected-window) (dirvish-get-all 'root-window t))))

(defun dirvish-wdired-mode-ad (&rest _)
  "Advisor function for `wdired-change-to-wdired-mode'."
  (dired-move-to-end-of-filename t)
  (setq-local cursor-type '(bar 4))
  (remove-hook 'post-command-hook #'dirvish-update-body-h :local)
  (mapc (lambda (ov) (unless (eq ov 'dirvish-zoom) (remove-overlays (point-min) (point-max) ov t)))
        (mapcar #'car (dv-attributes-alist (dirvish-curr)))))

(defun dirvish-refresh-cursor-ad (fn &rest args)
  "Only apply FN with ARGS when editing filenames in dirvish."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-deletion-ad (fn &rest args)
  "Advice function for FN `dired-internal-do-deletions' with its ARGS."
  (let ((trash-directory (dirvish--get-trash-dir))) (apply fn args)))

(defun dirvish-find-file-ad (&rest _)
  "Quit current dirvish instance if inside one.
Use it as a `:before' advisor to target function."
  (let* ((dv (dirvish-curr))
         (dv-tran (and dv (dv-transient dv))))
    (if dv-tran
        (dirvish--end-transient dv-tran)
      (and dv (dirvish-deactivate dv)))))

(defun dirvish-ignore-ad (fn &rest args)
  "Only apply FN with ARGS outside of Dirvish."
  (unless (dirvish-curr) (apply fn args)))

(defun dirvish--add-advices ()
  "Add all advices listed in `dirvish-advice-alist'."
  (pcase-dolist (`(,file ,sym ,fn ,place) dirvish-advice-alist)
    (when (require file nil t) (advice-add sym (or place :around) fn))))

(defun dirvish--remove-advices ()
  "Remove all advices listed in `dirvish-advice-alist'."
  (pcase-dolist (`(,_ ,sym ,fn) dirvish-advice-alist) (advice-remove sym fn)))

(provide 'dirvish-advices)
;;; dirvish-advices.el ends here
