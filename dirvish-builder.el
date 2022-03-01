;;; dirvish-builder.el ---  Build a Dirvish layout in a window or frame -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides functions for building a dirvish layout.

;;; Code:

(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(require 'dirvish-core)
(require 'dirvish-updater)
(require 'dirvish-preview)
(require 'dirvish-options)
(require 'dirvish-helpers)

(defun dirvish-rebuild-parents-h (frame)
  "Rebuild dirvish layout in FRAME."
  (dirvish-reclaim frame)
  (when-let ((dv (and (dirvish-live-p) (dirvish-curr))))
    (unless (dv-transient dv) (dirvish-build))))

(defun dirvish-hide-details-h ()
  "Hide other parent windows when showing Dired details."
  (if dired-hide-details-mode (dirvish-build) (dirvish--enlarge)))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let ((dv (dirvish-curr)))
    (cond ((eobp) (forward-line -1)) ((bobp) (forward-line 1)))
    (dired-move-to-filename)
    (dirvish-body-update (dv-attributes-alist dv))
    (when-let ((filename (dired-get-filename nil t)))
      (setf (dv-index-path dv) filename)
      (dirvish-debounce 'layout (dirvish-mode-line-update) (dirvish-preview-update)))))

(defun dirvish-quit-h ()
  "Quit current Dirvish."
  (dirvish-deactivate (gethash dirvish--curr-name (dirvish-hash)))
  (and (string= (frame-parameter nil 'name) "dirvish-emacs") (delete-frame))
  (unless (window-parameter nil 'window-side) (switch-to-buffer dirvish-temp-buffer)))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function."
  (dired-revert)
  (dirvish-clean-preview-images (dired-get-marked-files))
  (dirvish-setup-dired-buffer)
  (dirvish-update-body-h))

(defun dirvish-setup (&optional keep-dired)
  "Default config for dirvish parent windows.
If KEEP-DIRED is specified, reuse the old Dired buffer."
  (unless keep-dired
    (dirvish-mode)
    (setq-local revert-buffer-function #'dirvish-revert)
    (dirvish-setup-dired-buffer))
  (set (make-local-variable 'face-remapping-alist) dirvish-face-remap-alist)
  (setq-local face-font-rescale-alist nil)
  (setq-local dired-hide-details-hide-symlink-targets nil) ;; See `dirvish--render-symlink-target'
  (setq cursor-type nil)
  (set-window-fringes nil 1 1)
  (when (bound-and-true-p all-the-icons-dired-mode)
    (all-the-icons-dired-mode -1)
    (setq-local tab-width 2))
  (when dirvish--child-entry (dired-goto-file dirvish--child-entry))
  (setq dirvish--vc-backend (ignore-errors (vc-responsible-backend default-directory)))
  (let (dired-hide-details-mode-hook) (dired-hide-details-mode t))
  (let* ((dv (dirvish-curr))
         (owp (dirvish-dired-p dv))
         (header-fn (dv-header-string-fn dv)))
    (dirvish-body-update (dv-attributes-alist dv))
    (push (selected-window) (dv-dired-windows dv))
    (push (current-buffer) (dv-dired-buffers dv))
    (setq-local dirvish--curr-name (dv-name dv))
    (setq mode-line-format (and owp dirvish-mode-line-format '((:eval (dirvish--format-mode-line)))))
    (setq header-line-format (and owp dirvish-header-string-function `((:eval (funcall #',header-fn))))))
  (add-hook 'window-buffer-change-functions #'dirvish-rebuild-parents-h nil :local)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil :local)
  (add-hook 'quit-window-hook #'dirvish-quit-h nil :local)
  (run-hooks 'dirvish-mode-hook))

(defun dirvish--build-parents (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (depth (if (dv-dedicated dv) 0 (dv-depth dv)))
         (i 0))
    (dirvish-setup dirvish--curr-name)
    (unless (dirvish-dired-p dv)
      (add-hook 'dired-hide-details-mode-hook #'dirvish-hide-details-h nil :local))
    (while (and (< i depth) (not (string= current parent)))
      (setq i (1+ i))
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent current))
      (setq parent (dirvish--get-parent parent)))
    (when (> depth 0)
      (let* ((remain (- 1 dirvish-preview-width dirvish-parent-max-width))
             (width (min (/ remain depth) dirvish-parent-max-width))
             (dired-after-readin-hook nil))
        (cl-dolist (parent-dir parent-dirs)
          (let* ((current (car parent-dir))
                 (parent (cdr parent-dir))
                 (win-alist `((side . left)
                              (inhibit-same-window . t)
                              (window-width . ,width)
                              (window-parameters . ((no-other-window . t)))))
                 (buffer (dired-noselect parent))
                 (window (display-buffer buffer `(dirvish--display-buffer . ,win-alist))))
            (with-selected-window window
              (setq-local dirvish--child-entry current)
              (dirvish-setup))))))))

(defun dirvish--build-preview (dv)
 "Create a window showing preview for DV."
  (let* ((inhibit-modification-hooks t)
         (buf (dirvish--get-util-buffer dv 'preview))
         (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
         (fringe 30)
         (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
    (set-window-fringes new-window fringe fringe nil t)
    (setf (dv-preview-window dv) new-window)))

(defun dirvish--build-header (dv)
  "Create a window showing header for DV."
  (when dirvish-header-style
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-util-buffer dv 'header))
           (win-alist `((side . above)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish--build-footer (dv)
  "Create a window showing footer for DV."
  (when dirvish-mode-line-format
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-util-buffer dv 'footer))
           (win-alist `((side . below)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish-build ()
  "Build dirvish layout."
  (let ((dv (dirvish-curr)))
    (unless (dirvish-dired-p dv)
      (delete-other-windows)
      (dirvish--build-preview dv)
      (dirvish--build-header dv)
      (dirvish--build-footer dv))
    (dirvish--build-parents dv)))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish :interactive nil)

(provide 'dirvish-builder)
;;; dirvish-builder.el ends here
