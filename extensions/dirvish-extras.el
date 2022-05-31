;;; dirvish-extras.el --- Extra commands, attributes, or preview dispatchers for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.3.21
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

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
;; - `dirvish-other-buffer'
;; - `dirvish-show-history'
;; - `dirvish-go-forward-history'
;; - `dirvish-go-backward-history'
;; - `dirvish-roam'
;; - `dirvish-switch-layout'
;;
;; Attributes
;; - `file-size'
;; - `vscode-icon'
;; - `all-the-icons'
;;
;; Mode-line segments
;; - `free-space'
;; - `file-link-number'
;; - `file-user'
;; - `file-group'
;; - `file-time'
;; - `file-size'
;; - `file-modes'
;; - `file-inode-number'
;; - `file-device-number'

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

(defcustom dirvish-vscode-icon-size 32
  "Icon (image pixel) size used for `vscode-icon' backend.
The value should be a integer between 23 to 128."
  :group 'dirvish :type 'integer)

(defcustom dirvish-layout-recipes
  '((0 0    0.4)   ;        | CURRENT | preview
    (0 0    0.8)   ;        | current | PREVIEW
    (1 0.08 0.8)   ; parent | current | PREVIEW
    (1 0.11 0.55)) ; parent | current | preview
  "Layout RECIPEs for `dirvish-switch-layout' command.
RECIPE has the same form as `dirvish-default-layout'."
  :group 'dirvish
  :type '(repeat (list (integer :tag "number of parent windows")
                       (float :tag "max width of parent windows")
                       (float :tag "width of preview window"))))

(defcustom dirvish-time-format-string "%R-%x"
  "FORMAT-STRING for `file-time' mode line segment.
This value is passed to function `format-time-string'."
  :group 'dirvish :type 'string)

(defface dirvish-free-space
  '((t (:inherit font-lock-constant-face)))
  "Face used for `free-space' mode-line segment."
  :group 'dirvish)

(defface dirvish-file-link-number
  '((t (:inherit font-lock-constant-face)))
  "Face used for file link number mode-line segment."
  :group 'dirvish)

(defface dirvish-file-user-id
  '((t (:inherit font-lock-preprocessor-face)))
  "Face used for file size attributes / mode-line segment."
  :group 'dirvish)

(defface dirvish-file-group-id
  '((t (:inherit dirvish-file-user-id)))
  "Face used for file group id mode-line segment."
  :group 'dirvish)

(defface dirvish-file-time
  '((t (:inherit font-lock-string-face)))
  "Face used for file access/modify/change time mode-line segment."
  :group 'dirvish)

(defface dirvish-file-size
  '((t (:inherit completions-annotations)))
  "Face used for display file size attributes / mode-line segment."
  :group 'dirvish)

(defface dirvish-file-modes
  '((t (:inherit font-lock-builtin-face)))
  "Face used for file mode (privilege) mode-line segment."
  :group 'dirvish)

(defface dirvish-file-inode-number
  '((t (:inherit dirvish-file-link-number)))
  "Face used for file inode number mode-line segment."
  :group 'dirvish)

(defface dirvish-file-device-number
  '((t (:inherit dirvish-file-link-number)))
  "Face used for filesystem device number mode-line segment."
  :group 'dirvish)

(defun dirvish--get-file-size-or-count (name attrs)
  "Get file size of file NAME from ATTRS."
  (let ((type (file-attribute-type attrs)))
    (cond ((dirvish-prop :remote) (or (file-attribute-size attrs) "?"))
          ((stringp type)
           (let ((count
                  (dirvish-attribute-cache name :f-count
                    (condition-case nil
                        (number-to-string
                         (- (length (directory-files name nil nil t)) 2))
                      (file-error 'file)))))
             (if (eq count 'file)
                 (file-size-human-readable
                  (file-attribute-size (file-attributes name)))
               count)))
          (type
           (let ((count
                  (dirvish-attribute-cache name :f-count
                    (condition-case nil
                        (number-to-string
                         (- (length (directory-files name nil nil t)) 2))
                      (file-error 'no-permission)))))
             (if (eq count 'no-permission) "?" count)))
          (t (file-size-human-readable (or (file-attribute-size attrs) 0))))))

(defun dirvish--format-file-attr (attr-name)
  "Return a string of cursor file's attribute ATTR-NAME."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (attr-getter (intern (format "file-attribute-%s" attr-name)))
              (attr-face (intern (format "dirvish-file-%s" attr-name)))
              (attr-val (and attrs (funcall attr-getter attrs))))
    (propertize (format "%s" attr-val) 'face attr-face)))

(dirvish-define-attribute all-the-icons
  "File icons provided by `all-the-icons.el'."
  (:left (+ (length dirvish-icon-delimiter) 2))
  (let* ((offset `(:v-adjust ,dirvish-all-the-icons-offset))
         (height `(:height ,dirvish-all-the-icons-height))
         (face (cond (hl-face `(:face ,hl-face))
                     ((eq dirvish-all-the-icons-palette 'all-the-icons) nil)
                     (t `(:face ,dirvish-all-the-icons-palette))))
         (icon-attrs (append face offset height))
         (icon (if (eq (car f-type) 'dir)
                   (apply #'all-the-icons-icon-for-dir f-name icon-attrs)
                 (apply #'all-the-icons-icon-for-file f-name icon-attrs)))
         (icon-str (concat icon (propertize dirvish-icon-delimiter 'face hl-face)))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'after-string icon-str) ov))

(dirvish-define-attribute vscode-icon
  "File icons provided by `vscode-icon.el'."
  (:left (1+ (length dirvish-icon-delimiter)))
  (let* ((vscode-icon-size dirvish-vscode-icon-size)
         (icon-info
          (dirvish-attribute-cache f-name :vscode-icon
            (let ((default-directory dirvish--vscode-icon-directory))
              (if (eq (car f-type) 'dir)
                  (let* ((base-name (file-name-base f-name))
                         (icon-base (or (cdr (assoc base-name vscode-icon-dir-alist))
                                        base-name))
                         (icon-path (vscode-icon-dir-exists-p icon-base))
                         closed-icon opened-icon)
                    (if icon-path
                        (progn
                          (setq closed-icon
                                (vscode-icon-create-image icon-path))
                          (setq opened-icon
                                (vscode-icon-create-image
                                 (expand-file-name
                                  (format "folder_type_%s_opened.png" icon-base)))))
                      (setq closed-icon
                            (vscode-icon-create-image
                             (expand-file-name "default_folder.png")))
                      (setq opened-icon
                            (vscode-icon-create-image
                             (expand-file-name "default_folder_opened.png"))))
                    (cons closed-icon opened-icon))
                (vscode-icon-file f-name)))))
         (icon (cond ((eq (car f-type) 'file) icon-info)
                     ((dirvish--subtree-expanded-p) (cdr icon-info))
                     (t (car icon-info))))
         (ov (make-overlay (1- f-beg) f-beg)))
    (overlay-put ov 'display icon)
    (overlay-put ov 'before-string (propertize " " 'face hl-face))
    (overlay-put ov 'after-string
                 (propertize dirvish-icon-delimiter 'face hl-face)) ov))

(dirvish-define-attribute file-size
  "Show file size or directories file count at right fringe."
  (:if (and (eq (dv-root-window dv) (selected-window))
        dired-hide-details-mode)
       :right 6)
  (let* ((depth (* dirvish--subtree-prefix-len (dirvish--subtree-depth)))
         (width (window-width))
         (info (dirvish--get-file-size-or-count f-name f-attrs))
         (f-size-str (let* ((spc (concat info " ")) (len (- 6 (length spc))))
                       (if (> len 0) (concat (make-string len ?\ ) spc) spc)))
         (f-size-len (length f-size-str))
         (f-base-str (buffer-substring f-beg f-end))
         (f-base-len (string-width f-base-str))
         (remained (- width f-size-len depth (car dirvish--attrs-width)))
         (ov-pos (if (> remained f-base-len)
                     l-end
                   (let ((pos f-beg) (vis-str ""))
                     (while (< (string-width vis-str) remained)
                       (setq pos (1+ pos))
                       (setq vis-str (buffer-substring f-beg pos)))
                     pos)))
         (face (or hl-face 'dirvish-file-size))
         (spc (propertize " " 'display
                          `(space :align-to (- right-fringe ,f-size-len)) 'face face))
         (ov (make-overlay ov-pos ov-pos)))
    (add-face-text-property 0 f-size-len face t f-size-str)
    (overlay-put ov 'after-string (concat spc f-size-str)) ov))

(dirvish-define-mode-line free-space
  "Amount of free space on `default-directory''s file system."
  (let ((free-space (or (dirvish-prop :free-space)
                        (get-free-disk-space default-directory) "")))
    (dirvish-prop :free-space free-space)
    (format " %s %s " (propertize free-space 'face 'dirvish-free-space)
            (propertize "free" 'face 'font-lock-doc-face))))

(dirvish-define-mode-line file-link-number
  "Number of links to file."
  (dirvish--format-file-attr 'link-number))

(dirvish-define-mode-line file-user
  "User name of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (uid (and attrs (file-attribute-user-id attrs)))
              (uname (if (dirvish-prop :remote) uid (user-login-name uid))))
    (propertize uname 'face 'dirvish-file-user-id)))

(dirvish-define-mode-line file-group
  "Group name of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (gid (and attrs (file-attribute-group-id attrs)))
              (gname (if (dirvish-prop :remote) gid (group-name gid))))
    (propertize gname 'face 'dirvish-file-group-id)))

(dirvish-define-mode-line file-time
  "Last modification time of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (f-mtime (file-attribute-modification-time attrs))
              (time-string
               (if (dirvish-prop :remote) f-mtime
                 (format-time-string dirvish-time-format-string f-mtime))))
    (format "%s" (propertize time-string 'face 'dirvish-file-time))))

(dirvish-define-mode-line file-size
  "File size of files or file count of directories."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (size (and attrs (dirvish--get-file-size-or-count f-name attrs))))
    (format "%s" (propertize size 'face 'dirvish-file-size))))

(dirvish-define-mode-line file-modes
  "File modes, as a string of ten letters or dashes as in ls -l."
  (dirvish--format-file-attr 'modes))

(dirvish-define-mode-line file-inode-number
  "File's inode number, as a nonnegative integer."
  (dirvish--format-file-attr 'inode-number))

(dirvish-define-mode-line file-device-number
  "Filesystem device number, as an integer."
  (dirvish--format-file-attr 'device-number))

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
         (idx (cl-position
               (or (dirvish-prop :fd-dir) (dired-current-directory))
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
  (let ((hint (propertize
               "Copied: " 'face 'font-lock-builtin-face)))
    (message "%s" (format "%s%s" hint string))))

;;;###autoload
(defun dirvish-copy-file-true-path ()
  "Copy truename of (maybe) symlink file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (file-truename (dired-get-filename nil t))))

;;;###autoload
(defun dirvish-copy-file-name (&optional multi-line)
  "Copy filename of marked files.
If MULTI-LINE, make every name occupy a separate line."
  (interactive "P")
  (let* ((files (dired-get-marked-files t))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

;;;###autoload
(defun dirvish-copy-file-path (&optional multi-line)
  "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a separate line."
  (interactive "P")
  (let* ((files (dired-get-marked-files))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

;;;###autoload
(defun dirvish-copy-file-directory ()
  "Copy directory name of file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (expand-file-name default-directory)))

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
  "Browse all directories using `fd' command.
This command takes a while to index all the directories the first
time you run it.  After the indexing, it fires up instantly."
  (interactive)
  (unless (executable-find "fd")
    (user-error "Dirvish: install `fd' to use this command"))
  (let* ((command "fd -H -td -0 . /")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (dirvish--append-metadata 'file files-raw))
         (file (completing-read "Goto: " files)))
    (dired-jump nil file)))

;;;###autoload
(defun dirvish-switch-layout (&optional recipe)
  "Switch Dirvish layout according to RECIPE.
If RECIPE is not provided, switch to the recipe next to the
current layout defined in `dirvish-layout-recipes'."
  (interactive)
  (cl-loop
   with dv = (let ((dv (dirvish-curr)))
               (unless dv (user-error "Not in a Dirvish session"))
               (unless (dv-layout dv)
                 (dirvish-toggle-fullscreen)
                 (user-error "Dirvish: entering fullscreen")) dv)
   with old-recipe = (dv-layout dv)
   with recipes = (if recipe (list recipe) dirvish-layout-recipes)
   with l-length = (length recipes)
   for idx from 1
   for recipe in recipes
   when (or (eq idx l-length) (equal old-recipe recipe))
   return
   (let* ((new-idx (if (> idx (1- l-length)) 0 idx))
          (new-recipe (nth new-idx recipes)))
     (setf (dv-layout dv) new-recipe)
     (setf (dv-last-fs-layout dv) new-recipe)
     (dirvish-build dv))))

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here
