;;; dirvish-extras.el --- Extra commands, attributes, or preview dispatchers for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This library provided:
;;
;; Commands
;; - `dirvish-find-file-true-path'
;; - `dirvish-copy-file-name'
;; - `dirvish-copy-file-path'
;; - `dirvish-copy-file-directory'
;; - `dirvish-rename-space-to-underscore'
;; - `dirvish-roam'
;;
;; Attributes
;; - `file-size' attribute at right fringe
;; - `vs-icon' attribute
;; - `all-the-icons' attribute

;;; Code:

(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(declare-function vscode-icon-can-scale-image-p "vscode-icon")
(declare-function vscode-icon-file "vscode-icon")
(declare-function vscode-icon-dir-exists-p "vscode-icon")
(declare-function vscode-icon-create-image "vscode-icon")
(defvar vscode-icon-size)
(defvar vscode-icon-dir-alist)
(defvar vscode-icon-dir)
(require 'all-the-icons nil t)
(require 'vscode-icon nil t)
(require 'dirvish-commands)

(defvar dirvish--vscode-icon-directory
  (concat (and (boundp 'vscode-icon-dir) vscode-icon-dir)
          (if (and (fboundp 'vscode-icon-can-scale-image-p) (vscode-icon-can-scale-image-p)) "128/" "23/")))
(require 'dired)
(require 'dirvish-helpers)

(defcustom dirvish-icon-delimiter " "
  "A string attached to the icon (for both backends)."
  :group 'dirvish :type 'string)

(defcustom dirvish-all-the-icons-offset 0.01
  "Icon's vertical offset used for `all-the-icons' backend.
Set it to nil to use the default offset from `all-the-icons'."
  :group 'dirvish :type '(choice (float nil)))

(defcustom dirvish-all-the-icons-height nil
  "Icon height used for `all-the-icons' backend.
The height of the icon is scaled to this value (try 0.8).
Set it to nil to use the default height from `all-the-icons'."
  :group 'dirvish :type '(choice (float nil)))

(defcustom dirvish-all-the-icons-palette 'all-the-icons
  "Coloring style used for file `all-the-icons' backend.
Values are interpreted as follows:
- 'all-the-icons, meaning let `all-the-icons.el' to do the coloring.
- A face that is used for all the icons.
- nil, inherit face at point."
  :group 'dirvish :type '(choice face symbol nil))

(defcustom dirvish-vscode-icon-size 32
  "Icon (image pixel) size used for `vscode-icon' backend.
The value should be a integer between 23 to 128."
  :group 'dirvish :type 'integer)
(defface dirvish-file-size-face
  '((t (:inherit font-lock-doc-face)))
  "Face for file size overlays."
  :group 'dirvish)

(dirvish-define-attribute all-the-icons
  :left (+ (length dirvish-icon-delimiter) 2)
  :form
  (let* ((offset `(:v-adjust ,dirvish-all-the-icons-offset))
         (height `(:height ,dirvish-all-the-icons-height))
         (face (unless (eq dirvish-all-the-icons-palette 'all-the-icons)
                 `(:face ,dirvish-all-the-icons-palette)))
         (icon-attrs (append face offset height))
         (icon (if (file-directory-p f-name)
                   (apply #'all-the-icons-icon-for-dir f-name icon-attrs)
                 (apply #'all-the-icons-icon-for-file f-name icon-attrs)))
         (icon-str (concat icon dirvish-icon-delimiter))
         (ov (make-overlay (1- f-beg) f-beg)))
    (when-let (hl-face (fg (face-attribute hl-face :foreground))
                       (bg (face-attribute hl-face :background)))
      (add-face-text-property 0 (length icon-str) `(:background ,bg :foreground ,fg) t icon-str))
    (overlay-put ov 'after-string icon-str) ov))

(dirvish-define-attribute vscode-icon
  :left (1+ (length dirvish-icon-delimiter))
  :form
  (let* ((vscode-icon-size dirvish-vscode-icon-size)
         (icon-info
          (dirvish-get-attribute-create f-name :vscode-icon nil
            (let ((default-directory dirvish--vscode-icon-directory))
              (if (file-directory-p f-name)
                  (let* ((base-name (file-name-base f-name))
                         (icon-base (or (cdr (assoc base-name vscode-icon-dir-alist)) base-name))
                         (icon-path (vscode-icon-dir-exists-p icon-base))
                         closed-icon opened-icon)
                    (if icon-path
                        (progn
                          (setq closed-icon (vscode-icon-create-image icon-path))
                          (setq opened-icon (vscode-icon-create-image
                                             (expand-file-name (format "folder_type_%s_opened.png" icon-base)))))
                      (setq closed-icon (vscode-icon-create-image (expand-file-name "default_folder.png")))
                      (setq opened-icon (vscode-icon-create-image (expand-file-name "default_folder_opened.png"))))
                    (cons closed-icon opened-icon))
                (vscode-icon-file f-name)))))
         (icon (cond ((not (file-directory-p f-name)) icon-info)
                     ((dirvish--subtree-expanded-p) (cdr icon-info))
                     (t (car icon-info))))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'display icon)
    (overlay-put ov 'before-string (propertize " " 'face hl-face))
    (overlay-put ov 'after-string (propertize dirvish-icon-delimiter 'face hl-face)) ov))

(dirvish-define-attribute file-size
  :if (and (eq (dv-root-window dv) (selected-window)) dired-hide-details-mode)
  :right 6
  :form
  (unless (file-directory-p f-name)
    (let* ((depth (* 2 (dirvish--get-subtree-depth)))
           (width (window-width))
           (f-size-raw (file-size-human-readable (if f-attrs (file-attribute-size f-attrs) 0)))
           (f-size-str (let* ((str-spc (concat f-size-raw " ")) (len (- 6 (length str-spc))))
                         (if (> len 0) (concat (make-string len ?\ ) str-spc) str-spc)))
           (f-size-len (length f-size-str))
           (f-base-str (buffer-substring f-beg f-end))
           (f-base-len (dirvish--actual-string-length f-base-str))
           (remained (- width f-size-len depth (car dirvish--attrs-width)))
           (ov-pos (if (> remained f-base-len)
                       f-end
                     (let ((pos f-beg) (vis-str ""))
                       (while (< (dirvish--actual-string-length vis-str) remained)
                         (setq pos (1+ pos))
                         (setq vis-str (buffer-substring f-beg pos)))
                       pos)))
           (face (or hl-face 'dirvish-file-size-face))
           (spc (propertize " " 'display `(space :align-to (- right-fringe ,f-size-len)) 'face face))
           (ov (make-overlay ov-pos ov-pos)))
      (add-face-text-property 0 f-size-len face t f-size-str)
      (overlay-put ov 'after-string (concat spc f-size-str)) ov)))

;;;###autoload
(defun dirvish-show-history ()
  "Open a target directory from `dirvish--history-ring'."
  (interactive)
  (unless (ring-p dirvish--history-ring) (user-error "Dirvish: history tracking has been disabled"))
  (let* ((history-w/metadata (dirvish--append-metadata 'file (ring-elements dirvish--history-ring)))
         (result (completing-read "Recently visited: " history-w/metadata)))
      (when result (dirvish-find-file result))))

;;;###autoload
(defun dirvish-other-buffer ()
  "Switch to the most recently visited dirvish buffer."
  (interactive)
  (unless (ring-p dirvish--history-ring) (user-error "Dirvish: history tracking has been disabled"))
  (dirvish-find-file (ring-ref dirvish--history-ring 1)))

;;;###autoload
(defun dirvish-find-file-true-path ()
  "Open truename of (maybe) symlink file under the cursor."
  (interactive)
  (dired-jump nil (file-truename (dired-get-filename))))

;;;###autoload
(defun dirvish-copy-file-name ()
  "Copy filename under the cursor."
  (interactive)
  (message "Copied file NAME: %s" (dired-copy-filename-as-kill)))

;;;###autoload
(defun dirvish-copy-file-path ()
  "Copy filename under the cursor."
  (interactive)
  (message "Copied file PATH: %s" (kill-new (dired-get-filename nil t))))

;;;###autoload
(defun dirvish-copy-file-directory ()
  "Copy the current directory's (`default-directory''s) absolute path."
  (interactive)
  (message "Copied file DIRECTORY: %s" (kill-new (expand-file-name default-directory))))

;;;###autoload
(defun dirvish-rename-space-to-underscore ()
  "Rename marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (derived-mode-p 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (revert-buffer))
    (user-error "Not in a Dired buffer")))

;;;###autoload
(defun dirvish-roam ()
  "Browse all directories using `fd' command."
  (interactive)
  (unless (executable-find "fd") (user-error "Dirvish: install `fd' to use this command"))
  (let* ((command "fd -H -td -0 . /")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (dirvish--append-metadata 'file files-raw))
         (file (completing-read "Goto: " files)))
    (dired-jump nil file)))

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here
