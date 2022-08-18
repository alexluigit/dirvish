;;; dirvish-widgets.el --- Core mode line segments and attributes for dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This library provided:
;;
;; Attributes
;; - `file-size'
;;
;; Mode-line segments
;; - `path'
;; - `symlink'
;; - `omit'
;; - `index'
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

;; A small value (< 7) would cause line skipping on Emacs 28-, see #77
(defconst dirvish--file-size-str-len 8)

(defun dirvish--count-file-size (fileset)
  "Return file size of FILESET in bytes."
  (cl-labels ((f-name (f) (if (file-directory-p f)
                              (directory-files-recursively f ".*" nil t)
                            f))
              (f-size (f) (file-attribute-size (file-attributes f))))
    (cl-reduce #'+ (mapcar #'f-size (flatten-tree (mapcar #'f-name fileset))))))

(defun dirvish--file-size-add-spaces (str)
  "Fill file size STR with leading spaces."
  (let* ((spc (concat str (if (dirvish-prop :gui) " " "")))
         (len (- dirvish--file-size-str-len (length spc))))
    (if (> len 0) (concat (make-string len ?\ ) spc) spc)))

(defun dirvish--get-file-size-or-count (name attrs)
  "Get file size of file NAME from ATTRS."
  (let ((type (file-attribute-type attrs)))
    (cond ((dirvish-prop :tramp)
           (dirvish--file-size-add-spaces
            (or (file-attribute-size attrs) "?")))
          ((stringp type)
           (let ((count
                  (dirvish-attribute-cache name :f-count
                    (condition-case nil
                        (dirvish--file-size-add-spaces
                         (number-to-string
                          (- (length (directory-files name nil nil t)) 2)))
                      (file-error 'file)))))
             (if (eq count 'file)
                 (dirvish-attribute-cache name :f-size
                   (dirvish--file-size-add-spaces
                    (file-size-human-readable
                     (file-attribute-size (file-attributes name)))))
               count)))
          (type
           (let ((count
                  (dirvish-attribute-cache name :f-count
                    (condition-case nil
                        (dirvish--file-size-add-spaces
                         (number-to-string
                          (- (length (directory-files name nil nil t)) 2)))
                      (file-error 'no-permission)))))
             (if (eq count 'no-permission) " NOPERM " count)))
          (t (dirvish--file-size-add-spaces
              (dirvish-attribute-cache name :f-size
                (file-size-human-readable (or (file-attribute-size attrs) 0))))))))

(defun dirvish--format-file-attr (attr-name)
  "Return a string of cursor file's attribute ATTR-NAME."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (attrs (dirvish-attribute-cache f-name :builtin))
              (attr-getter (intern (format "file-attribute-%s" attr-name)))
              (attr-face (intern (format "dirvish-file-%s" attr-name)))
              (attr-val (and attrs (funcall attr-getter attrs))))
    (propertize (format "%s" attr-val) 'face attr-face)))

;;;; Attributes

(dirvish-define-attribute file-size
  "Show file size or directories file count at right fringe."
  (:if (and (dirvish-prop :root) dired-hide-details-mode)
       :width (1+ dirvish--file-size-str-len))
  (let* ((str (dirvish--get-file-size-or-count f-name f-attrs))
         (ov-pos (if (> remain f-wid) l-end (+ f-beg remain)))
         (face (or hl-face 'dirvish-file-size))
         (dp-spec `(space :align-to (- right-fringe
                                       ,dirvish--file-size-str-len
                                       ,(if (dirvish-prop :gui) 0 2))))
         (spc (propertize " " 'display dp-spec 'face face))
         (ov (make-overlay ov-pos ov-pos)))
    (setq str (concat spc str))
    (add-face-text-property 0 (1+ dirvish--file-size-str-len) face t str)
    (overlay-put ov 'after-string str)
    ov))

;;;; Mode line segments

(defun dirvish--register-path-seg (segment path face)
  "Register mode line path SEGMENT with target PATH and FACE."
  (propertize
   segment 'face face 'mouse-face 'highlight
   'help-echo "mouse-1: visit this directory"
   'keymap `(header-line keymap
                         (mouse-1 . (lambda (_ev)
                                      (interactive "e")
                                      (dirvish-find-entry-ad ,path))))))

(dirvish-define-mode-line path
  "Path of file under the cursor."
  (let* ((index (dired-current-directory))
         (face (if (dirvish--window-selected-p dv) 'dired-header 'shadow))
         (abvname (abbreviate-file-name (file-local-name index)))
         (rmt (and (dirvish-prop :tramp)
                   (tramp-file-name-handler 'file-remote-p index)))
         (host (propertize (if rmt (concat " " (substring rmt 1)) "")
                           'face 'font-lock-builtin-face))
         (segs (nbutlast (split-string abvname "/")))
         (scope (pcase (car segs)
                  ("~" (dirvish--register-path-seg
                        " ⌂ " (concat rmt "~/") face))
                  ("" (dirvish--register-path-seg
                       " ∀ " (concat rmt "/") face))))
         (path (cl-loop for idx from 2
                        for sp = (format
                                  "%s%s" (or rmt "")
                                  (mapconcat #'concat (seq-take segs idx) "/"))
                        for s in (cdr segs) concat
                        (format "%s%s" (if (eq idx 2) "" " ⋗ ")
                                (dirvish--register-path-seg s sp face)))))
    (replace-regexp-in-string "%" "%%%%" (format "%s%s%s " host scope path))))

(dirvish-define-mode-line sort
  "Current sort criteria."
  (let* ((switches (split-string dired-actual-switches))
         (crit (cond (dired-sort-inhibit "DISABLED")
                     ((member "--sort=none" switches) "none")
                     ((member "--sort=time" switches) "time")
                     ((member "--sort=version" switches) "version")
                     ((member "--sort=size" switches) "size")
                     ((member "--sort=extension" switches) "extension")
                     ((member "--sort=width" switches) "width")
                     (t "name")))
         (time (cond ((member "--time=use" switches) "use")
                     ((member "--time=ctime" switches) "ctime")
                     ((member "--time=birth" switches) "birth")
                     (t "mtime")))
         (rev (if (member "--reverse" switches) "↓" "↑")))
    (format " %s %s|%s "
            (propertize rev 'face 'font-lock-doc-markup-face)
            (propertize crit 'face 'font-lock-type-face)
            (propertize time 'face 'font-lock-doc-face))))

(dirvish-define-mode-line omit
  "A `dired-omit-mode' indicator."
  (and (bound-and-true-p dired-omit-mode) (propertize "Omit" 'face 'font-lock-negation-char-face)))

(dirvish-define-mode-line symlink
  "Show the truename of symlink file under the cursor."
  (when-let* ((name (or (dirvish-prop :child) (dired-get-filename nil t)))
              (f-name (file-local-name name))
              (truename (cdr (dirvish-attribute-cache f-name :type))))
    (format " %s %s "
            (propertize "→" 'face 'font-lock-comment-delimiter-face)
            (propertize truename 'face 'dired-symlink))))

(dirvish-define-mode-line index
  "Current file's index and total files count."
  (let ((cur-pos (- (line-number-at-pos (point)) 1))
        (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
    (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold))))

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

(provide 'dirvish-widgets)
;;; dirvish-widgets.el ends here
