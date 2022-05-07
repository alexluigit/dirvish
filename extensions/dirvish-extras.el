;;; dirvish-extras.el --- Extra commands, attributes, or preview dispatchers for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.2.0
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
;; - `dirvish-total-file-size'
;; - `dirvish-rename-space-to-underscore'
;; - `dirvish-roam'
;;
;; Attributes
;; - `file-size'
;; - `expanded-state'
;; - `vscode-icon'
;; - `all-the-icons'

;;; Code:

(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-icon-for-dir "all-the-icons")
(declare-function all-the-icons-octicon "all-the-icons")
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
- 'all-the-icons, meaning let `all-the-icons.el' to do the coloring.
- A face that is used for all the icons.
- nil, inherit face at point."
  :group 'dirvish :type '(choice face symbol nil))

(defcustom dirvish-vscode-icon-size 32
  "Icon (image pixel) size used for `vscode-icon' backend.
The value should be a integer between 23 to 128."
  :group 'dirvish :type 'integer)

(defvar dirvish--expanded-state-fn nil)
(defcustom dirvish-expanded-state-style 'chevron
  "Icon/string used for directory expanded state.
The value can be one of: `plus', `arrow', `chevron'."
  :group 'dirvish :type 'symbol
  :set
  (lambda (k v)
    (set k v)
    (setq dirvish--expanded-state-fn
          (pcase v
            ('plus (lambda (s f) (propertize (if s "-" "+") 'face f)))
            ('arrow (lambda (s f) (propertize (if s "▾" "▸") 'face f)))
            ('chevron
             (if (require 'all-the-icons nil t)
                 (lambda (s f) (all-the-icons-octicon
                           (format "chevron-%s" (if s "down" "right"))
                           :height (* (or dirvish-all-the-icons-height 1) 0.8)
                           :v-adjust 0.1 :face f))
               (set k 'arrow)
               (lambda (s f) (propertize (if s "▾" "▸") 'face f))
               (user-error "Dirvish: chevron expanded state require package `all-the-icons'")))))))

(defface dirvish-file-size-face
  '((t (:inherit font-lock-doc-face)))
  "Face for file size overlays."
  :group 'dirvish)

(defface dirvish-expanded-state-face
  '((t (:inherit font-lock-doc-face)))
  "Face for expanded state overlays."
  :group 'dirvish)

(dirvish-define-attribute all-the-icons
  :left (+ (length dirvish-icon-delimiter) 2)
  :if (or dirvish--dir-local-p
          (memq 'extras dirvish-enabled-features-on-remote))
  :form
  (let* ((offset `(:v-adjust ,dirvish-all-the-icons-offset))
         (height `(:height ,dirvish-all-the-icons-height))
         (face (cond (hl-face `(:face ,hl-face))
                     ((eq dirvish-all-the-icons-palette 'all-the-icons) nil)
                     (t `(:face ,dirvish-all-the-icons-palette))))
         (icon-attrs (append face offset height))
         (icon (if (eq f-type 'dir)
                   (apply #'all-the-icons-icon-for-dir f-name icon-attrs)
                 (apply #'all-the-icons-icon-for-file f-name icon-attrs)))
         (icon-str (concat icon (propertize dirvish-icon-delimiter 'face hl-face)))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'after-string icon-str) ov))

(dirvish-define-attribute vscode-icon
  :left (1+ (length dirvish-icon-delimiter))
  :if (or dirvish--dir-local-p
          (memq 'extras dirvish-enabled-features-on-remote))
  :form
  (let* ((vscode-icon-size dirvish-vscode-icon-size)
         (icon-info
          (dirvish-attribute-cache f-name :vscode-icon
            (let ((default-directory dirvish--vscode-icon-directory))
              (if (eq f-type 'dir)
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
  :if (and (or dirvish--dir-local-p
               (memq 'extras dirvish-enabled-features-on-remote))
           (eq (dv-root-window dv) (selected-window)) dired-hide-details-mode)
  :right 6
  :form
  (unless (eq f-type 'dir)
    (let* ((depth (* dirvish--subtree-prefix-len (dirvish--get-subtree-depth)))
           (width (window-width))
           (f-size-str
            (concat (dirvish-attribute-cache f-name :file-size
                      (let* ((info (file-size-human-readable
                                    (if f-attrs (file-attribute-size f-attrs) 0)))
                             (spc (concat info " "))
                             (len (- 6 (length spc))))
                        (if (> len 0) (concat (make-string len ?\ ) spc) spc)))))
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

(dirvish-define-attribute expanded-state
  :if (and (or dirvish--dir-local-p
               (memq 'extras dirvish-enabled-features-on-remote))
           (eq (dv-root-window dv) (selected-window)))
  :left 1
  :form
  (let ((state-str (if (eq f-type 'dir)
                       (funcall dirvish--expanded-state-fn
                                (dirvish--subtree-expanded-p)
                                'dirvish-expanded-state-face)
                     (propertize " ")))
        (ov (make-overlay (1+ l-beg) (1+ l-beg))))
    (when hl-face
      (add-face-text-property 0 1 hl-face t state-str))
    (overlay-put ov 'after-string state-str) ov))

;;;###autoload
(defun dirvish-show-history ()
  "Open a target directory from `dirvish--history-ring'."
  (interactive)
  (let* ((history-w/metadata
          (dirvish--append-metadata
           'file (ring-elements dirvish--history-ring)))
         (result (completing-read "Recently visited: " history-w/metadata)))
      (when result (dirvish-find-file result))))

;;;###autoload
(defun dirvish-other-buffer ()
  "Switch to the most recently visited dirvish buffer."
  (interactive)
  (dirvish-find-file (ring-ref dirvish--history-ring 1)))

;;;###autoload
(defun dirvish-go-forward-history (&optional arg)
  "Navigate to next ARG directory in history.
ARG defaults to 1."
  (interactive "^p")
  (or arg (setq arg 1))
  (let* ((dirs (reverse
                (mapcar #'car (dv-root-dir-buf-alist (dirvish-curr)))))
         (len (length dirs))
         (idx (cl-position (dired-current-directory)
                           dirs :test #'equal))
         (new-idx (+ idx arg)))
    (cond ((>= new-idx len)
           (dirvish-find-file (nth (- len 1) dirs))
           (message "Dirvish: reached the end of history"))
          ((< new-idx 0)
           (dirvish-find-file (nth 0 dirs))
           (message "Dirvish: reached the beginning of history"))
          (t (dirvish-find-file (nth new-idx dirs))))))

;;;###autoload
(defun dirvish-go-backward-history (&optional arg)
  "Navigate to last ARG directory in history.
ARG defaults to -1."
  (interactive "^p")
  (dirvish-go-forward-history (- 0 (or arg 1))))

;;;###autoload
(defun dirvish-find-file-true-path ()
  "Open truename of (maybe) symlink file under the cursor."
  (interactive)
  (dired-jump nil (file-truename (dired-get-filename nil t))))

(defun dirvish--kill-and-echo (string)
  "Echo last killed STRING."
  (kill-new string)
  (message "%s" (format "%s%s" (propertize "Copied: " 'face 'font-lock-builtin-face) string)))

;;;###autoload
(defun dirvish-copy-file-true-path ()
  "Copy truename of (maybe) symlink file under the cursor."
  (interactive)
  (dirvish--kill-and-echo (file-truename (dired-get-filename nil t))))

;;;###autoload
(defun dirvish-copy-file-name ()
  "Copy filename under the cursor."
  (interactive)
  (dirvish--kill-and-echo (dired-get-filename t t)))

;;;###autoload
(defun dirvish-copy-file-path ()
  "Copy filename under the cursor."
  (interactive)
  (dirvish--kill-and-echo (dired-get-filename nil t)))

;;;###autoload
(defun dirvish-copy-file-directory ()
  "Copy the current directory's (`default-directory''s) absolute path."
  (interactive)
  (dirvish--kill-and-echo (expand-file-name default-directory)))

;;;###autoload
(defun dirvish-total-file-size (&optional fileset)
  "Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'."
  (interactive)
  (let* ((fileset (or fileset (dired-get-marked-files)))
         (count (propertize (number-to-string (length fileset))
                            'face 'font-lock-builtin-face))
         (size (file-size-human-readable (dirvish--get-filesize fileset))))
    (message "%s" (format "Total size of %s entries: %s" count size))))

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
