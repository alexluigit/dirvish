;;; dirvish-builder.el ---  Build a Dirvish layout in a window or frame -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provides functions for building a dirvish layout.

;;; Code:

(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(declare-function dired-filter--describe-filters "dired-filter")
(require 'dirvish-preview)
(defvar dired-filter-mode)
(defvar dired-filter-show-filters)
(defvar dired-filter-revert)
(defvar fd-dired-input-fd-args)
(eval-when-compile
  (require 'subr-x)
  (require 'find-dired))
(when (require 'dired-filter nil t)
  (setq dired-filter-show-filters nil)
  (setq dired-filter-revert 'always))

(dirvish-define-attribute hl-line
  :form
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end)))) (overlay-put ov 'face hl-face) ov)))

;; This hack solves 2 issues:
;; 1. Hide " -> " arrow of symlink files as well.
;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
(dirvish-define-attribute symlink-target
  :if (and dired-hide-details-mode (default-value 'dired-hide-details-hide-symlink-targets))
  :form
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end))) (overlay-put ov 'invisible t) ov)))

(defun dirvish-default-header-string-fn ()
  "Compose header string."
  (when-let ((dv (dirvish-curr)))
    (let* ((index (dv-index-path dv))
           (file-path (or (file-name-directory index) ""))
           (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
           (path-regex (concat (getenv "HOME") "/\\|\\/$"))
           (path-tail (replace-regexp-in-string path-regex "" file-path))
           (file-name (file-name-nondirectory index)))
      (format " %s %s %s"
              (propertize (if path-prefix-home "~" ":"))
              (propertize path-tail 'face 'dired-mark)
              (propertize file-name 'face 'font-lock-constant-face)))))

(defun dirvish-find-dired-header-string-fn ()
  "Return a string showing current `find/fd' command args."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (when-let ((args (or (bound-and-true-p fd-dired-input-fd-args) find-args)))
      (format " %s [%s] at %s"
              (propertize "FD:" 'face 'bold)
              (propertize args 'face 'font-lock-string-face)
              (propertize default-directory 'face 'dired-header)))))

(defun dirvish--mode-line-sorter ()
  "Return a string showing current Dired file sort criteria."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (format " %s %s "
            (propertize "Sort:" 'face 'bold)
            (or (and dired-sort-inhibit (propertize "inhibited" 'face 'font-lock-warning-face))
                (propertize (car (dv-sort-criteria (dirvish-curr))) 'face 'font-lock-type-face)))))

(defun dirvish--mode-line-filter ()
  "Return a string showing active Dired file filter."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (cond ((bound-and-true-p dired-filter-mode)
           (format " %s %s " (propertize "Filters:" 'face 'bold)
                   (dired-filter--describe-filters)))
          (dired-omit-mode (propertize "[Omit]" 'face 'bold)))))

(defun dirvish--mode-line-index ()
  "Return a string showing index in a Dirvish buffer."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (let ((cur-pos (- (line-number-at-pos (point)) 1))
          (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
      (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold)))))

(defun dirvish-rebuild-parents-h (frame)
  "Rebuild dirvish layout in FRAME."
  (dirvish-reclaim frame)
  (when-let ((dv (and (dirvish-live-p) (dirvish-curr))))
    (unless (dv-transient dv) (dirvish-build))))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let ((dv (dirvish-curr)))
    (cond ((eobp) (forward-line -1)) ((bobp) (forward-line 1)))
    (dired-move-to-filename)
    (dirvish--render-attributes dv)
    (when-let ((filename (dired-get-filename nil t)))
      (setf (dv-index-path dv) filename)
      (dirvish-debounce layout
        (with-current-buffer (dirvish--get-util-buffer dv 'footer) (force-mode-line-update))
        (with-current-buffer (dirvish--get-util-buffer dv 'header) (force-mode-line-update))
        (dirvish-preview-update)))))

(defun dirvish-quit-h ()
  "Quit current Dirvish."
  ;; FIXME: dv should be always accessible here
  (if-let ((dv (gethash dirvish--curr-name (dirvish-hash))))
      (dirvish-deactivate dv)
    (kill-current-buffer))
  (switch-to-buffer (dirvish--ensure-temp-buffer)))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function."
  (dired-revert)
  (dirvish-clean-preview-images (dired-get-marked-files))
  (dirvish--hide-dired-header)
  (dirvish-update-body-h))

(defun dirvish-setup (&optional keep-dired)
  "Default config for dirvish parent windows.
If KEEP-DIRED is specified, reuse the old Dired buffer."
  (unless keep-dired
    (dirvish-mode)
    (setq-local revert-buffer-function #'dirvish-revert)
    (dirvish--hide-dired-header))
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
    (dirvish--render-attributes dv)
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
    (when (window-parameter (selected-window) 'window-side)
      (setq-local window-size-fixed 'width))
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
                 (buffer (dirvish--buffer-for-dir dv parent t))
                 (window (display-buffer buffer `(dirvish--display-buffer . ,win-alist))))
            (with-selected-window window
              (setq-local dirvish--child-entry current)
              (dirvish-setup)
              ;; always hide details in parent windows
              (let (dired-hide-details-mode-hook) (dired-hide-details-mode t)))))))))

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

(defun dirvish--noselect (dir)
  "Return the Dirvish buffer at DIR, do not select it."
  (or dir (setq dir default-directory))
  (let ((dv (dirvish-activate (dirvish-new :depth -1))))
    (with-current-buffer (dirvish--buffer-for-dir dv dir)
      (dirvish-build)
      (current-buffer))))

(defun dirvish--noselect-async-sentinel (proc _state)
  "Sentinel for `dirvish--noselect-async''s Dired PROC."
  (with-current-buffer (process-buffer proc)
    (let (buffer-read-only)
      (delete-region (point-min) (+ (point-min) (process-get proc 'len)))
      (delete-region (progn (goto-char (point-max)) (forward-line -1) (point)) (point-max)))
    (dirvish--hide-dired-header)
    (dirvish-debounce layout (dirvish-update-body-h))))

(defun dirvish--noselect-async (entry dired-switches)
  "Open ENTRY with DIRED-SWITCHES asynchronously."
  (with-current-buffer (generate-new-buffer "Dirvish-cache")
    (let* ((info "[DIRVISH] caching directory...\n")
           (buf (prog1 (current-buffer) (insert info)))
           (async-cmd `(with-current-buffer (dired-noselect ,entry ,dired-switches)
                         (buffer-substring-no-properties (point-min) (point-max))))
           (proc (start-process (buffer-name buf) buf "emacs" "-q" "-batch" "--eval"
                                (format "(message \"%%s\" %S)" async-cmd))))
      (setq default-directory entry)
      (setq dired-subdir-alist (list (cons entry (point-min-marker))))
      (dirvish-mode)
      (dirvish-setup t)
      (set-process-sentinel proc #'dirvish--noselect-async-sentinel)
      (process-put proc 'len (length info))
      (setq revert-buffer-function #'ignore) ; TODO
      (setq-local dirvish--dired-async-p t)
      buf)))

(defun dirvish--buffer-for-dir (dv entry &optional parent)
  "Return the root or PARENT buffer in DV for ENTRY.
If the buffer is not available, create it with `dired-noselect'."
  (let* ((dir-buf (if parent (dv-parent-dir-buf-alist dv) (dv-root-dir-buf-alist dv)))
         (buffer (alist-get entry dir-buf nil nil #'equal))
         (sorter (cdr (dv-sort-criteria dv)))
         (switches (string-join (list (dv-ls-switches dv) sorter) " ")))
    (unless buffer
      (let ((files-count (length (directory-files entry nil nil t))))
        (if (> files-count dirvish-async-listing-threshold)
            (setq buffer (dirvish--noselect-async entry switches))
          (setq buffer (dired-noselect entry switches))))
      (push (cons entry buffer)
            (if parent (dv-parent-dir-buf-alist dv) (dv-root-dir-buf-alist dv))))
    buffer))

(defun dirvish-build ()
  "Build dirvish layout."
  (let ((dv (dirvish-curr)))
    (unless (dirvish-dired-p dv)
      (let ((ignore-window-parameters t)) (delete-other-windows))
      (dirvish--build-preview dv)
      (dirvish--build-header dv)
      (dirvish--build-footer dv))
    (dirvish--build-parents dv)))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish :interactive nil)

(provide 'dirvish-builder)
;;; dirvish-builder.el ends here
