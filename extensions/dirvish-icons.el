;;; dirvish-icons.el --- Icon support for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Integrate `all-the-icons', `nerd-icons', and `vscode-icon' with Dirvish.

;;; Code:

(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function nerd-icons-icon-for-dir "nerd-icons")
(declare-function vscode-icon-can-scale-image-p "vscode-icon")
(declare-function vscode-icon-file "vscode-icon")
(declare-function vscode-icon-dir-exists-p "vscode-icon")
(declare-function vscode-icon-create-image "vscode-icon")
(defvar vscode-icon-size)
(defvar vscode-icon-dir-alist)
(defvar vscode-icon-dir)
(require 'all-the-icons nil t)
(require 'vscode-icon nil t)
(require 'dirvish)

(defvar dirvish--vscode-icon-directory
  (concat (and (boundp 'vscode-icon-dir) vscode-icon-dir)
          (if (and (fboundp 'vscode-icon-can-scale-image-p) (vscode-icon-can-scale-image-p)) "128/" "23/")))

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
- all-the-icons, meaning let `all-the-icons.el' to do the coloring.
- A face that is used for all the icons.
- nil, inherit face at point."
  :group 'dirvish :type '(choice face symbol nil))

(defcustom dirvish-nerd-icons-offset 0.00
  "Icon's vertical offset used for `nerd-icons' backend.
Set it to nil to use the default offset from `nerd-icons'."
  :group 'dirvish :type '(choice (float nil)))

(defcustom dirvish-nerd-icons-height nil
  "Icon height used for `nerd-icons' backend.
The height of the icon is scaled to this value (try 0.8).
Set it to nil to use the default height from `nerd-icons'."
  :group 'dirvish :type '(choice (float nil)))

(defcustom dirvish-nerd-icons-palette 'nerd-icons
  "Coloring style used for file `nerd-icons' backend.
Values are interpreted as follows:
- nerd-icons, meaning let `nerd-icons.el' to do the coloring.
- A face that is used for all the icons.
- nil, inherit face at point."
  :group 'dirvish :type '(choice face symbol nil))

(defcustom dirvish-vscode-icon-size 32
  "Icon (image pixel) size used for `vscode-icon' backend.
The value should be a integer between 23 to 128."
  :group 'dirvish :type 'integer)

(dirvish-define-attribute all-the-icons
  "File icons provided by `all-the-icons.el'."
  :width (+ (length dirvish-icon-delimiter) 2)
  (let* ((offset `(:v-adjust ,dirvish-all-the-icons-offset))
         (height `(:height ,dirvish-all-the-icons-height))
         (face (cond (hl-face `(:face ,hl-face))
                     ((eq dirvish-all-the-icons-palette 'all-the-icons) nil)
                     (t `(:face ,dirvish-all-the-icons-palette))))
         (icon-attrs (append face offset height))
         (icon (if (eq (car f-type) 'dir)
                   (apply #'all-the-icons-icon-for-dir f-name icon-attrs)
                 (apply #'all-the-icons-icon-for-file f-str icon-attrs)))
         (icon-str (concat icon (propertize dirvish-icon-delimiter 'face hl-face)))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'after-string icon-str)
    `(ov . ,ov)))

(dirvish-define-attribute nerd-icons
  "File icons provided by `nerd-icons.el'."
  :width (+ (length dirvish-icon-delimiter) 2)
  (let* ((offset `(:v-adjust ,dirvish-nerd-icons-offset))
         (height `(:height ,dirvish-nerd-icons-height))
         (face (cond (hl-face `(:face ,hl-face))
                     ((eq dirvish-nerd-icons-palette 'nerd-icons) nil)
                     (t `(:face ,dirvish-nerd-icons-palette))))
         (icon-attrs (append face offset height))
         (icon (if (eq (car f-type) 'dir)
                   (apply #'nerd-icons-icon-for-dir f-name icon-attrs)
                 (apply #'nerd-icons-icon-for-file f-str icon-attrs)))
         (icon-str (concat icon (propertize dirvish-icon-delimiter 'face hl-face)))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'after-string icon-str)
    `(ov . ,ov)))

(dirvish-define-attribute vscode-icon
  "File icons provided by `vscode-icon.el'."
  :width (1+ (length dirvish-icon-delimiter))
  (let* ((vscode-icon-size dirvish-vscode-icon-size)
         (icon
          (dirvish-attribute-cache f-name :vscode-icon
            (let ((default-directory dirvish--vscode-icon-directory))
              (if (eq (car f-type) 'dir)
                  (let* ((base (file-name-sans-extension f-str))
                         (i-base (or (cdr (assoc base vscode-icon-dir-alist))
                                     base))
                         (i-path (vscode-icon-dir-exists-p i-base)))
                    (vscode-icon-create-image
                     (or i-path (expand-file-name "default_folder.png"))))
                (vscode-icon-file f-name)))))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'display icon)
    (overlay-put ov 'before-string (propertize " " 'face hl-face))
    (overlay-put ov 'after-string
                 (propertize dirvish-icon-delimiter 'face hl-face))
    `(ov . ,ov)))

(provide 'dirvish-icons)
;;; dirvish-icons.el ends here
