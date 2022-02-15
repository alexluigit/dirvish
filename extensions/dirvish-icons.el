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
(declare-function vscode-icon-for-file "vscode-icon")
(defvar vscode-icon-size)
(defconst dirvish-icon-v-offset 0.01)
(defvar-local dirvish--vscode-icon-alist nil)
(require 'dired)
(require 'dirvish-structs)

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

(defun dirvish--vscode-icon-for-file (file)
  "Get vscode-icon for FILE."
  (let ((icon (alist-get file dirvish--vscode-icon-alist nil nil #'string=)))
    (unless icon
      (setq icon (vscode-icon-for-file file))
      (push (cons file icon) dirvish--vscode-icon-alist))
    icon))

;;;###autoload (autoload 'dirvish--render-all-the-icons-body "dirvish-icons")
;;;###autoload (autoload 'dirvish--render-all-the-icons-line "dirvish-icons")
(dirvish-define-attribute all-the-icons (beg hl-face) :lineform
  (let* ((entry (dired-get-filename 'relative 'noerror))
         (offset `(:v-adjust ,dirvish-icon-v-offset))
         (icon-face (unless (eq dirvish-icon-palette 'all-the-icons) `(:face ,dirvish-icon-palette)))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p entry)
                   (apply #'all-the-icons-icon-for-dir entry icon-attrs)
                 (apply #'all-the-icons-icon-for-file entry icon-attrs)))
         (icon-str (concat icon dirvish-icon-delimiter))
         (ov (make-overlay (1- beg) beg)))
    (when-let (hl-face (fg (face-attribute hl-face :foreground))
                       (bg (face-attribute hl-face :background)))
      (add-face-text-property 0 (length icon-str) `(:background ,bg :foreground ,fg) t icon-str))
    (overlay-put ov 'dirvish-all-the-icons t)
    (overlay-put ov 'after-string icon-str)))

;;;###autoload (autoload 'dirvish--render-vscode-icon-body "dirvish-icons")
;;;###autoload (autoload 'dirvish--render-vscode-icon-line "dirvish-icons")
(dirvish-define-attribute vscode-icon (beg hl-face) :lineform
  (let* ((entry (dired-get-filename nil 'noerror))
         (vscode-icon-size dirvish-icon-size)
         (icon (dirvish--vscode-icon-for-file entry))
         (after-str dirvish-icon-delimiter)
         (ov (make-overlay (1- beg) beg)))
    (when hl-face (setq after-str (propertize after-str 'face hl-face)))
    (overlay-put ov 'display icon)
    (overlay-put ov 'dirvish-vscode-icon t)
    (overlay-put ov 'after-string after-str)))

(provide 'dirvish-icons)
;;; dirvish-icons.el ends here
