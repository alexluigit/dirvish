;;; dirvish-emerge.el --- Pin files you are interested in at top -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.8.14
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Allows important files emerged (pinned) at top in Dirvish buffers.

;;; Code:

(declare-function dirvish-emerge--menu "dirvish-emerge")
(require 'dirvish)

(defun dirvish-emerge-safe-groups-p (groups)
  "Return t if GROUPS is a list and has less than 100 items."
  (and (listp groups) (< (length groups) 100)))

(defcustom dirvish-emerge-groups '()
  "Default emerge groups applied to all Dirvish buffer.
The value is an alist of (NAME . (TYPE . VALUE)) where NAME is a
string to designate the name and display title of the group, TYPE
is a symbol in one of `predicate', `extensions', or `regex'.  The
corresponding VALUEs (criteria) for these types are:

- `predicate': a symbol that points to a predicate
- `extensions': one or more filename extensions
- `regex': a regular expression

The predicates are defined by `dirvish-emerge-define-predicate'.

Here is a sample value for this variable.

\((\"Recent\" (predicate . `recent-files-2h'))
 (\"README\" (regex . \"README\"))
 (\"PDF\"    (extensions \"pdf\"))
 (\"LaTeX\"  (extensions \"tex\" \"bib\")))

When `dirvish-emerge-mode' is enabled in the buffer, the fileset
in the buffer are separated and rearranged by the following groups:

1. files modified within 2 hours
2. files whose name can be matched by \"README\"
3. files whose extension is \"pdf\"
4. files whose extension is \"tex\" or \"bib\"
5. other files

You can set this variable globally, a more appropriate way would
be set it directory locally though.  You can compose and save
this variable to .dir-locals.el through `dirvish-emerge-menu'."
  :group 'dirvish :type 'alist)
(put 'dirvish-emerge-groups 'safe-local-variable #'dirvish-emerge-safe-groups-p)

(defcustom dirvish-emerge-max-file-count 20000
  "Inhibit auto grouping in big directories.
If file count of the directory is greater than this value,
automatic grouping is disabled even if `dirvish-emerge-mode' is
turned on in the buffer."
  :group 'dirvish :type 'integer)

(defface dirvish-emerge-group-title
  '((t :inherit dired-ignored))
  "Face used for emerge group title."
  :group 'dirvish)

(defclass dirvish-emerge-group (transient-infix)
  ((hide     :initarg :hide)
   (selected :initarg :selected)
   (recipe   :initarg :recipe))
  "[Experimental] Class for Dirvish emerge groups.")

(cl-defmethod transient-format-key ((obj dirvish-emerge-group))
  "Format key for 'dirvish-emerge-group' instance OBJ."
  (let ((key (oref obj key))
        (sel (oref obj selected)))
    (propertize key 'face (if sel 'transient-value 'transient-key))))

(cl-defmethod transient-format-description ((obj dirvish-emerge-group))
  "Format description for 'dirvish-emerge-group' instance OBJ."
  (let ((desc (oref obj description))
        (sel (oref obj selected)))
    (propertize desc 'face (and sel 'transient-value))))

(cl-defmethod transient-format-value ((obj dirvish-emerge-group))
  "Format value for 'dirvish-emerge-group' instance OBJ."
  (pcase-let* ((`(,type . ,val) (oref obj recipe))
               (face (if (oref obj hide) 'font-lock-comment-face
                       'transient-argument)))
    (pcase type
      ('regex (propertize (format "regex: %s" val) 'face face))
      ('extensions (propertize (format "extensions: %s" (mapconcat #'concat val ","))
                               'face face))
      ('predicate (propertize (format "form: %s" val) 'face face)))))

(cl-defmethod transient-infix-read ((obj dirvish-emerge-group))
  "Read value from 'dirvish-emerge-group' instance OBJ."
  (oset obj value (list (oref obj description) (oref obj recipe)
                        (oref obj hide) (oref obj selected))))

(cl-defmethod transient-infix-set ((obj dirvish-emerge-group) _value)
  "Set value for 'dirvish-emerge-group' instance OBJ."
  (if-let ((sel (oref obj selected)))
      (dirvish-emerge-read-recipe (oref obj recipe) obj)
    (oset obj selected t)))

(defvar dirvish-emerge--max-pred-name-len 0)
(defvar dirvish-emerge--available-preds '())

(defmacro dirvish-emerge-define-predicate (name docstring &rest body)
  "Define a group predicate NAME with BODY.
DOCSTRING is the documention of the predicate.
The predicate takes the following arguments:

- `local-name': output from (file-name-nondirectory FILE)
- `full-name': output from (dired-get-filename)
- `type': a cons of (TYPE . SYM-TARGET).  TYPE is either `dir' or
  `file'.  SYM-TARGET is the symlink target as a string when the
  file is a symlink, otherwise nil.
- `attrs': output from (file-attributes FILE)

The predicate is consumed by `dirvish-emerge-groups'."
  (declare (indent defun) (doc-string 2))
  `(let* ((fn (lambda (local-name full-name type attrs)
                (ignore local-name full-name type attrs) ,@body))
          (pair (assq ',name dirvish-emerge--available-preds))
          (val (cons ',name (cons fn ,docstring))))
     (setf dirvish-emerge--max-pred-name-len
           (max dirvish-emerge--max-pred-name-len
                (length (format "%s" ',name))))
     (if pair
         (setcdr (assq ',name dirvish-emerge--available-preds) val)
       (push val dirvish-emerge--available-preds))))

(dirvish-emerge-define-predicate recent-files-2h
  "File modified within 2 hours."
  (let ((mtime (file-attribute-modification-time attrs)))
    (and (listp mtime)
         (< (float-time (time-subtract (current-time) mtime)) 7200))))

(dirvish-emerge-define-predicate recent-files-today
  "File modified today."
  (let ((mtime (file-attribute-modification-time attrs)))
    (and (listp mtime)
         (< (float-time (time-subtract (current-time) mtime)) 86400))))

(cl-defgeneric dirvish-emerge-read-recipe (recipe &optional obj)
  "Read RECIPE from user input and optionally save it to OBJ.")

(cl-defmethod dirvish-emerge-read-recipe ((recipe (head regex)) &optional obj)
  "Read RECIPE from user input and optionally save it to OBJ."
  (let* ((deft (cdr recipe))
         (regex (read-regexp
                 (format "Change regex to (defaults to %s): " deft) deft)))
    (if obj (oset obj recipe `(regex . ,regex)) regex)))

(cl-defmethod dirvish-emerge-read-recipe ((recipe (head extensions)) &optional obj)
  "Read RECIPE from user input and optionally save it to OBJ."
  (let* ((prompt "Input one or more extensions: ")
         (cands
          (cl-remove-if-not (lambda (i) (and i (> (length i) 0)))
                            (mapcar #'file-name-extension
                                    (hash-table-keys dirvish--attrs-hash))))
         (exts (completing-read-multiple
                prompt cands nil nil (mapconcat #'concat (cdr recipe) ","))))
    (if obj (oset obj recipe `(extensions . ,@exts)) exts)))

(cl-defmethod dirvish-emerge-read-recipe ((recipe (head predicate)) &optional obj)
  "Read RECIPE from user input and optionally save it to OBJ."
  (ignore recipe)
  (let* ((table dirvish-emerge--available-preds)
         (coll (dirvish--append-metadata
                (lambda (i)
                  (let ((item (intern (format "%s" i))))
                    (concat
                     (make-string
                      (- dirvish-emerge--max-pred-name-len (length i) -8) ?\s)
                     (cddr (assq item table)))))
                table))
         (pred (completing-read "Predicate: " coll)))
    (if obj (oset obj recipe `(predicate . ,(read pred))) (read pred))))

(defsubst dirvish-emerge--make-pred (recipe)
  "Make predicate function from RECIPE."
  (pcase-let ((`(,type . ,val) recipe))
    (pcase type
      ('regex
       `(lambda (local-name _ _ _) (string-match ,val local-name)))
      ('extensions
       (let ((exts (format "\\.\\(%s\\)$" (mapconcat #'concat val "\\|"))))
         `(lambda (local-name _ _ _) (string-match ,exts local-name))))
      ('predicate
       (cadr (assq (cdr recipe) dirvish-emerge--available-preds))))))

(defun dirvish-emerge--create-infix
    (ifx description recipe &optional selected hide)
  "Create a 'dirvish-emerge-group' infix IFX.
DESCRIPTION, RECIPE, SELECTED and HIDE are inserted into the
corresponding slots."
  (eval `(transient-define-infix ,ifx ()
           :class 'dirvish-emerge-group
           :recipe ',recipe
           :selected ,selected
           :hide ,hide
           :description ,description)))

(defun dirvish-emerge--create-infixes ()
  "Define and collect emerge groups from 'dirvish-emerge-groups'."
  (cl-loop with len = (length dirvish-emerge-groups)
           for idx from 0
           for (desc recipe hide selected) in (seq-take dirvish-emerge-groups 99)
           for ifx = (intern (format "dirvish-%s-infix"
                                     (replace-regexp-in-string " " "-" desc)))
           for key = (format (if (> len 10) "%02i" "%i") idx)
           collect (progn
                     (dirvish-emerge--create-infix
                      ifx desc recipe selected hide)
                     (list key ifx))))

(defun dirvish-emerge--ifx-apply ()
  "Apply emerge infixes in `transient-current-suffixes'."
  (let* ((ifxes (cl-loop for o in transient-current-suffixes
                         when (eq (type-of o) 'dirvish-emerge-group)
                         collect o))
         (preds (cl-loop for idx from 1 to (length ifxes)
                         for obj in ifxes
                         collect (cons idx (dirvish-emerge--make-pred (oref obj recipe)))))
         (groups (cl-loop for o in ifxes
                          collect (list (oref o description) (oref o recipe)
                                        (oref o hide) (oref o selected)))))
    (setq-local dirvish-emerge-groups groups)
    (dirvish-prop :emerge-preds preds)
    (dirvish-emerge--apply t)))

(defun dirvish-emerge--ifx-unselect ()
  "Unselect selected emerge groups."
  (cl-loop for obj in transient-current-suffixes
           when (eq (type-of obj) 'dirvish-emerge-group)
           do (oset obj selected nil)))

(defun dirvish-emerge--ifx-toggle-hiding ()
  "Hide selected emerge groups."
  (cl-loop for obj in transient-current-suffixes
           when (and (eq (type-of obj) 'dirvish-emerge-group)
                     (oref obj selected))
           do (oset obj hide (not (oref obj hide)))))

(defun dirvish-emerge--ifx-add ()
  "Add a new emerge group to `transient-current-suffixes'."
  (let ((type (read (completing-read
                     "Select group type: "
                     '(extensions predicate regex) nil t)))
        (names (mapcar #'car dirvish-emerge-groups))
        (idx 1)
        (default "Anon-1")
        recipe title)
    (while (member default names)
      (setq idx (1+ idx))
      (setq default (format "Anon-%s" idx)))
    (setq recipe (dirvish-emerge-read-recipe (cons type nil)))
    (setq title (read-string "Group title: " default))
    (push (list title (cons type recipe)) dirvish-emerge-groups)
    (dirvish-emerge-menu)))

(defun dirvish-emerge--ifx-remove ()
  "Remove an emerge group from `transient-current-suffixes'."
  (cl-loop for obj in transient-current-suffixes
           when (and (eq (type-of obj) 'dirvish-emerge-group)
                     (oref obj selected))
           do (setf dirvish-emerge-groups
                    (assoc-delete-all
                     (oref obj description) dirvish-emerge-groups #'equal)))
  (dirvish-emerge-menu))

(defun dirvish-emerge--ifx-promote (&optional demote)
  "Shift selected emerge groups the highest position.
If DEMOTE, shift them to the lowest instead."
  (cl-loop with sel = ()
           for obj in transient-current-suffixes
           when (and (eq (type-of obj) 'dirvish-emerge-group)
                     (oref obj selected))
           do (progn (push obj sel)
                     (setf dirvish-emerge-groups
                           (assoc-delete-all
                            (oref obj description)
                            dirvish-emerge-groups #'equal)))
           finally do
           (let* ((sel (cl-loop for o in (reverse sel) collect
                                (list (oref o description) (oref o recipe)
                                      (oref o hide) (oref o selected))))
                  (new-groups (if demote (append dirvish-emerge-groups sel)
                                (append sel dirvish-emerge-groups))))
             (setf dirvish-emerge-groups new-groups)))
  (dirvish-emerge-menu))

(defun dirvish-emerge--ifx-read ()
  "Read groups from .dir-locals.el."
  (dirvish-emerge--readin-groups-1)
  (dirvish-emerge-menu))

(defun dirvish-emerge--ifx-write ()
  "Write groups to .dir-locals.el."
  (add-dir-local-variable
   'dirvish-mode 'dirvish-emerge-groups
   (cl-loop for o in transient-current-suffixes
            when (eq (type-of o) 'dirvish-emerge-group) collect
            (list (oref o description) (oref o recipe)
                  (oref o hide) (oref o selected)))))

(defun dirvish-emerge--readin-groups-1 ()
  "Helper for `dirvish-emerge--readin-groups'."
  (hack-dir-local-variables)
  (when-let ((vals (or (and (local-variable-if-set-p 'dirvish-emerge-groups)
                            (buffer-local-value
                             'dirvish-emerge-groups (current-buffer)))
                       (cdr (assq 'dirvish-emerge-groups
                                  file-local-variables-alist))
                       (default-value 'dirvish-emerge-groups))))
    (hack-one-local-variable 'dirvish-emerge-groups vals)
    (dirvish-prop :emerge-preds
      (cl-loop for idx from 1 to (length vals)
               for (_desc recipe) in vals collect
               (cons idx (dirvish-emerge--make-pred recipe))))))

(defun dirvish-emerge--readin-groups (&optional _dv _entry buffer)
  "Readin emerge groups in BUFFER for session DV."
  (with-current-buffer (or buffer (current-buffer))
    (unless (dirvish-prop :fd-dir)
      (dirvish-emerge--readin-groups-1))))

(defun dirvish-emerge--format-group-title (desc)
  "Format group title by DESC in Dirvish buffer."
  (format "%s%s%s\n"
          (propertize " " 'font-lock-face
                      '(:inherit dirvish-emerge-group-title
                                 :strike-through t))
          (propertize (if desc (format " %s " desc) " Others ")
                      'face 'dirvish-emerge-group-title)
          (propertize " " 'display '(space :align-to right)
                      'font-lock-face
                      '(:inherit dirvish-emerge-group-title
                                 :strike-through t))))

(defun dirvish-emerge--insert-group (group)
  "Insert GROUP to buffer."
  (cl-loop with (idx . files) = group
           with hide = (nth 2 (nth (1- idx) dirvish-emerge-groups))
           with desc = (nth 0 (nth (1- idx) dirvish-emerge-groups))
           with start = (let* ((pos (point))
                               (o (make-overlay pos pos)))
                          (prog1 pos
                            (when (and (not hide) (> (length files) 0))
                              (overlay-put o 'dirvish-emerge-title t)
                              (overlay-put o 'invisible t)
                              (overlay-put o 'after-string
                                           (dirvish-emerge--format-group-title desc)))))
           for file in (reverse files) do (insert file "\n")
           finally do (when hide
                        (let ((o (make-overlay start (point))))
                          (overlay-put o 'dirvish-emerge-hide t)
                          (overlay-put o 'invisible t)))))

(defun dirvish-emerge--apply-1 (preds)
  "Helper for `dirvish-emerge--apply'.
PREDS are locally composed predicates."
  (let ((beg (dirvish-prop :content-beginning))
        (end (point-max))
        (old-f (dirvish-prop :child))
        (idx-m (1+ (length preds)))
        buffer-read-only curr-dir groups)
    (setq groups (cl-loop for i from 1 to idx-m collect (cons i '())))
    (remove-overlays beg end 'dirvish-emerge-title t)
    (remove-overlays beg end 'dirvish-emerge-hide t)
    (save-excursion
      (goto-char beg)
      (setq curr-dir (file-local-name (dired-current-directory)))
      (while (< (point) end)
        (when-let ((f-beg (dired-move-to-filename))
                   (f-end (dired-move-to-end-of-filename)))
          (let* ((l-beg (line-beginning-position))
                 (l-end (line-end-position))
                 (local (buffer-substring-no-properties f-beg f-end))
                 (full (concat curr-dir local))
                 (type (dirvish-attribute-cache full :type))
                 (attrs (dirvish-attribute-cache full :builtin))
                 (match (cl-loop for (index . fn) in preds
                                 for match = (funcall fn local full type attrs)
                                 thereis (and match index))))
            (push (buffer-substring-no-properties l-beg l-end)
                  (alist-get (or match idx-m) groups))))
        (forward-line 1))
      (delete-region beg end)
      (goto-char beg)
      (mapc #'dirvish-emerge--insert-group groups))
    (dired-goto-file old-f)))

(defun dirvish-emerge--apply (&optional force)
  "Readin `dirvish-emerge-groups' and apply them.
When FORCE, `dirvish-emerge-max-file-count' is ignored."
  (when (or force (< (hash-table-count dirvish--attrs-hash)
                     dirvish-emerge-max-file-count))
    (dirvish-emerge--readin-groups)
    (when-let ((preds (dirvish-prop :emerge-preds)))
      (dirvish-emerge--apply-1 preds))))

;;;###autoload
(defun dirvish-emerge-menu ()
  "Manage pinned files in Dirvish."
  (interactive)
  (dirvish-emerge--readin-groups)
  (eval
   `(transient-define-prefix dirvish-emerge--menu ()
      "Configure current Dirvish session."
      [:description
       (lambda ()
         (let ((title "Configure Emerging Groups")
               (notes "Press the index (like \"1\") to select the group
Press again to set the value for the group"))
           (format "%s\n%s" (propertize title 'face '(:inherit dired-mark :underline t)
                                        'display '((height 1.2)))
                   (propertize notes 'face 'font-lock-doc-face))))
       ["Active groups:"
        ,@(if dirvish-emerge-groups
              (dirvish-emerge--create-infixes)
            (list '("+" "  No active groups, press + to add one"
                    (lambda () (interactive) (dirvish-emerge--ifx-add)))))]]
      ["Actions:"
       ("RET" "Apply current setup" (lambda () (interactive) (dirvish-emerge--ifx-apply)))
       ("c" "  Clear selection"
        (lambda () (interactive) (dirvish-emerge--ifx-unselect)) :transient t)
       ("h" "  Hide/show selected group"
        (lambda () (interactive) (dirvish-emerge--ifx-toggle-hiding)) :transient t)
       ("a" "  Add a group"
        (lambda () (interactive) (dirvish-emerge--ifx-add)))
       ("x" "  Remove selected groups"
        (lambda () (interactive) (dirvish-emerge--ifx-remove)))
       ("p" "  Promote selected groups"
        (lambda () (interactive) (dirvish-emerge--ifx-promote)))
       ("d" "  Demote selected groups"
        (lambda () (interactive) (dirvish-emerge--ifx-promote 'demote)))
       ("r" "  Read groups from .dir-locals.el"
        (lambda () (interactive) (dirvish-emerge--ifx-read)))
       ("w" "  Write groups to .dir-locals.el"
        (lambda () (interactive) (dirvish-emerge--ifx-write)))]))
  (dirvish-emerge--menu))

;;;###autoload
(define-minor-mode dirvish-emerge-mode
  "Toggle grouping of files in Dirvish."
  :group 'dirvish
  (if dirvish-emerge-mode
      (progn
        (add-hook 'dirvish-setup-hook #'dirvish-emerge--apply nil t)
        (when (derived-mode-p 'dirvish-mode) (dirvish-emerge--apply)))
    (remove-hook 'dirvish-setup-hook #'dirvish-emerge--apply t)
    (when (derived-mode-p 'dirvish-mode) (revert-buffer))))

(provide 'dirvish-emerge)
;;; dirvish-emerge.el ends here
