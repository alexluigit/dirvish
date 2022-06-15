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
;; - `dirvish-switch-layout'
;; - `dirvish-dwim'
;;
;; Attributes
;; - `file-size'
;; - `collapse'
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

(require 'dirvish)

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

(defun dirvish--count-file-size (fileset)
  "Return file size of FILESET in bytes."
  (cl-labels ((f-name (f) (if (file-directory-p f)
                              (directory-files-recursively f ".*" nil t)
                            f))
              (f-size (f) (file-attribute-size (file-attributes f))))
    (cl-reduce #'+ (mapcar #'f-size (flatten-tree (mapcar #'f-name fileset))))))

(defun dirvish--get-file-size-or-count (name attrs)
  "Get file size of file NAME from ATTRS."
  (let ((type (file-attribute-type attrs)))
    (cond ((dirvish-prop :tramp) (or (file-attribute-size attrs) "?"))
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
         (remained (- width f-size-len depth (dirvish-prop :width-l)))
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

(dirvish-define-attribute collapse
  "Collapse unique nested paths."
  (:if (and (eq (dv-root-window dv) (selected-window))
            (not (dirvish-prop :fd-dir))
            (or (not (dirvish-prop :tramp))
                (tramp-local-host-p (dirvish-prop :tramp)))))
  (let ((collapse
         (dirvish-attribute-cache f-name :collapse
           (let ((path f-name) files should-collapse)
             (while (and (file-directory-p path)
                         (setq files (ignore-errors (directory-files path t)))
                         (= 3 (length files)))
               (setq should-collapse t path (nth 2 files)))
             (cond ((and (eq (length files) 2) (not should-collapse))
                    (length (file-name-nondirectory f-name)))
                   (should-collapse path)
                   (t 'none))))))
    (unless (eq collapse 'none)
      (let* ((buffer-invisibility-spec nil)
             (default-directory (dired-current-directory))
             (head (stringp collapse))
             (path (and head (substring collapse (length default-directory))))
             (offset (if path (- (length path) (length (file-name-nondirectory path)))
                       collapse)))
        (when head
          (remove-overlays l-beg l-end 'dirvish-collapse t)
          (delete-region l-beg (1+ l-end))
          (insert "  ")
          (insert-directory path dired-actual-switches)
          (forward-line -1)
          (dired-align-file l-beg (1+ l-end)))
        (let ((ov (make-overlay f-beg (+ offset f-beg))))
          (prog1 (if head nil ov) (overlay-put ov 'face 'shadow)))))))

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
              (uname (if (dirvish-prop :tramp) uid (user-login-name uid))))
    (propertize uname 'face 'dirvish-file-user-id)))

(dirvish-define-mode-line file-group
  "Group name of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (gid (and attrs (file-attribute-group-id attrs)))
              (gname (if (dirvish-prop :tramp) gid (group-name gid))))
    (propertize gname 'face 'dirvish-file-group-id)))

(dirvish-define-mode-line file-time
  "Last modification time of file."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (f-mtime (file-attribute-modification-time attrs))
              (time-string
               (if (dirvish-prop :tramp) f-mtime
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
         (size (file-size-human-readable (dirvish--count-file-size fileset))))
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

;;;###autoload
(defun dirvish-dwim (&optional path)
  "Start a Dirvish session with optional PATH.
The session takes the whole frame when `one-window-p'."
  (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
  (let ((path (expand-file-name (or path default-directory))))
    (dirvish--reuse-session path)
    (if (dirvish-prop :dv)
        (and (one-window-p) (dirvish-toggle-fullscreen))
      (dirvish-new t :path path
        :layout (and (one-window-p) dirvish-default-layout)))))

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here
