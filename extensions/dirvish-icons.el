;;; dirvish-icons.el --- Icons support for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; `all-the-icons.el' and `vscode-icon.el' integration for Dirvish.

;;; Code:

(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(declare-function vscode-icon-can-scale-image-p "vscode-icon")
(declare-function dired-subtree--is-expanded-p "dired-subtree")
(defvar vscode-icon-size)
(defvar vscode-icon-dir)
(defconst dirvish--vscode-icon-directory
  (concat vscode-icon-dir (if (vscode-icon-can-scale-image-p) "128/" "23/")))
(defconst dirvish-icon-v-offset 0.01)
(require 'dired)
(require 'dirvish-core)

(defcustom dirvish-icon-delimiter " "
  "A string attached to the icon."
  :group 'dirvish :type 'string)

(defcustom dirvish-icon-size 32
  "Icon size used for vscode-icon backend."
  :group 'dirvish :type 'integer)

(define-obsolete-variable-alias 'dirvish-icon-monochrome 'dirvish-icon-palette "0.9.9")

(defcustom dirvish-icon-palette 'all-the-icons
  "Palette used for file all-the-icons backend.

Values are interpreted as follows:
- 'all-the-icons, meaning let `all-the-icons.el' to do the coloring.
- A face that is used for all the icons.
- nil, inherit face at point."
  :group 'dirvish :type '(choice face symbol nil))

;;;###autoload (autoload 'dirvish--render-all-the-icons-body "dirvish-icons")
;;;###autoload (autoload 'dirvish--render-all-the-icons-line "dirvish-icons")
(dirvish-define-attribute all-the-icons (f-name f-beg hl-face) :lineform
  (let* ((offset `(:v-adjust ,dirvish-icon-v-offset))
         (icon-face (unless (eq dirvish-icon-palette 'all-the-icons) `(:face ,dirvish-icon-palette)))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p f-name)
                   (apply #'all-the-icons-icon-for-dir f-name icon-attrs)
                 (apply #'all-the-icons-icon-for-file f-name icon-attrs)))
         (icon-str (concat icon dirvish-icon-delimiter))
         (ov (make-overlay (1- f-beg) f-beg)))
    (when-let (hl-face (fg (face-attribute hl-face :foreground))
                       (bg (face-attribute hl-face :background)))
      (add-face-text-property 0 (length icon-str) `(:background ,bg :foreground ,fg) t icon-str))
    (overlay-put ov 'dirvish-all-the-icons t)
    (overlay-put ov 'after-string icon-str)))

;;;###autoload (autoload 'dirvish--render-vscode-icon-body "dirvish-icons")
;;;###autoload (autoload 'dirvish--render-vscode-icon-line "dirvish-icons")
(dirvish-define-attribute vscode-icon (f-name f-beg hl-face) :lineform
  (let* ((vscode-icon-size dirvish-icon-size)
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
                     ((dirvish-get-attribute-create f-name :expanded nil nil) (cdr icon-info))
                     (t (car icon-info))))
         (after-str dirvish-icon-delimiter)
         (ov (make-overlay (1- f-beg) f-beg)))
    (when hl-face (setq after-str (propertize after-str 'face hl-face)))
    (overlay-put ov 'display icon)
    (overlay-put ov 'dirvish-vscode-icon t)
    (overlay-put ov 'after-string after-str)))

(provide 'dirvish-icons)
;;; dirvish-icons.el ends here
