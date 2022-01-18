;;; dirvish-builder.el ---  Build a Dirvish layout in a window or frame -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This library provides functions for building a dirvish layout.

;;; Code:

(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(require 'dirvish-updater)
(require 'dirvish-preview)
(require 'dirvish-structs)
(require 'dirvish-options)
(require 'dirvish-helpers)

(defun dirvish-update-viewport-h (_win _pos)
  "Refresh dirvish body attributes within viewport."
  (let ((buf (current-buffer)))
    ;; Do not update when current buffer exists in multiple windows
    (when (< (cl-count-if (lambda (w) (eq (window-buffer w) buf)) (window-list)) 2)
      (dirvish-body-update nil t))))

(defun dirvish-rebuild-parents-h (frame)
  "Rebuild dirvish layout in FRAME."
  (dirvish-reclaim frame)
  (when-let ((dv (dirvish-live-p)))
    (unless (dv-transient dv) (dirvish-build))))

(defun dirvish-hide-details-h ()
  "Hide other parent windows when showing Dired details."
  (if dired-hide-details-mode (dirvish-build) (dirvish--enlarge)))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function.  See
`dired-revert'."
  (dirvish-with-update t
    (dired-revert)
    (dirvish-setup-dired-buffer)))

(defun dirvish-setup (&optional keep-dired)
  "Default config for dirvish parent windows.
If KEEP-DIRED is specified, reuse the old Dired buffer."
  (unless keep-dired
    (dired-mode)
    (setq-local revert-buffer-function #'dirvish-revert)
    (dirvish-setup-dired-buffer))
  (set (make-local-variable 'face-remapping-alist)
       dirvish-parent-face-remap-alist)
  (setq-local face-font-rescale-alist nil)
  (setq cursor-type nil)
  (set-window-fringes nil 1 1)
  (when (bound-and-true-p all-the-icons-dired-mode)
    (all-the-icons-dired-mode -1)
    (setq-local tab-width 2))
  (when dirvish-child-entry (dired-goto-file dirvish-child-entry))
  (dirvish-body-update)
  (let* ((dv (dirvish-curr))
         (owp (dirvish-dired-p dv)))
    (push (selected-window) (dv-parent-windows dv))
    (push (current-buffer) (dv-parent-buffers dv))
    (setq-local dirvish--curr-name (dv-name dv))
    (setq mode-line-format (and owp dirvish-mode-line-format
                                '((:eval (dirvish-format-mode-line)))))
    (setq header-line-format (and owp dirvish-header-line-format
                                  '((:eval (format-mode-line dirvish-header-line-format))))))
  (dired-hide-details-mode t)
  (add-hook 'window-buffer-change-functions #'dirvish-rebuild-parents-h nil :local)
  (add-hook 'window-scroll-functions #'dirvish-update-viewport-h nil :local)
  (add-hook 'window-selection-change-functions #'dirvish-reclaim nil :local)
  (dirvish-mode))

(defun dirvish-build-parents ()
  "Create all dirvish parent windows."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (dv (dirvish-curr))
         (depth (dv-depth dv))
         (i 0))
    (dirvish-setup)
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
              (setq-local dirvish-child-entry current)
              (dirvish-setup))))))))

(defun dirvish-build-preview ()
  "Build dirvish preview window."
  (let* ((inhibit-modification-hooks t)
         (buf (dirvish--get-buffer 'preview))
         (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
         (fringe 30)
         (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
    (set-window-fringes new-window fringe fringe nil t)
    (setf (dv-preview-window (dirvish-curr)) new-window)))

(defun dirvish-build-header ()
  "Create a window showing dirvish header."
  (when (and dirvish-header-style dirvish-header-line-format)
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-buffer 'header))
           (win-alist `((side . above)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish-build-footer ()
  "Create a window showing dirvish footer."
  (when dirvish-mode-line-format
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-buffer 'footer))
           (win-alist `((side . below)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish-build ()
  "Build dirvish layout."
  (dirvish-with-update nil
    (unless (dirvish-dired-p)
      (delete-other-windows)
      (dirvish-build-preview)
      (dirvish-build-header)
      (dirvish-build-footer))
    (dirvish-build-parents)))

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map dired-mode-map)
    (define-key map [remap dired-up-directory]        'dirvish-up-directory)
    (define-key map [remap end-of-buffer]             'dirvish-go-bottom)
    (define-key map [remap beginning-of-buffer]       'dirvish-go-top)
    (define-key map [remap dired-sort-toggle-or-edit] 'dirvish-sort-by-criteria)
    (define-key map [remap quit-window]               'dirvish-quit)
    (define-key map [remap +dired/quit-all]           'dirvish-quit)
    map)
  "Dirvish mode map.")

(define-minor-mode dirvish-mode
  "For keymap only, it is not a command for users."
  :keymap dirvish-mode-map
  :group 'dirvish
  :interactive nil)

(provide 'dirvish-builder)
;;; dirvish-builder.el ends here
