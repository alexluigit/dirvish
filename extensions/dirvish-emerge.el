;;; dirvish-emerge.el --- Pin files you are interested in at top -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This extension allows user to pin important files at the top of Dirvish
;; buffers.  Type M-x dirvish-emerge-menu RET into a dirvish buffer to get
;; started.

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

Althought you can set this variable globally, a more appropriate
way would be set it directory locally.  In that case, it is
recommended to compose and save this variable to .dir-locals.el
by the help of `dirvish-emerge-menu'."
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

(defvar-local dirvish-emerge--group-overlays nil)

(cl-defmethod transient-format-key ((obj dirvish-emerge-group))
  "Format key for OBJ."
  (let ((key (oref obj key))
        (sel (oref obj selected)))
    (propertize key 'face (if sel 'transient-value 'transient-key))))

(cl-defmethod transient-format-description ((obj dirvish-emerge-group))
  "Format description for OBJ."
  (let ((desc (oref obj description))
        (sel (oref obj selected)))
    (propertize desc 'face (and sel 'transient-value))))

(cl-defmethod transient-format-value ((obj dirvish-emerge-group))
  "Format value for OBJ."
  (pcase-let* ((`(,type . ,val) (oref obj recipe))
               (face (if (oref obj hide) 'font-lock-comment-face
                       'transient-argument)))
    (pcase type
      ('regex (propertize (format "\"%s\"" val) 'face face))
      ('extensions (propertize (format "%s" (mapconcat #'concat val ","))
                               'face face))
      ('predicate (propertize "PRED" 'face face)))))

(cl-defmethod transient-infix-read ((obj dirvish-emerge-group))
  "Read value from OBJ."
  (oset obj value (list (oref obj description) (oref obj recipe)
                        (oref obj hide) (oref obj selected))))

(cl-defmethod transient-infix-set ((obj dirvish-emerge-group) _value)
  "Set value for OBJ."
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

(dirvish-emerge-define-predicate directories
  "Matches directories."
  (eq 'dir (car type)))

(dirvish-emerge-define-predicate files
  "Matches files."
  (eq 'file (car type)))

(dirvish-emerge-define-predicate symlinks
  "Matches symlimks."
  (cdr type))

;; Note the behavior of this predicate doesn't exactly match `file-executable-p'.
;; It checks if the owner of the file can execute it and not if the current
;; user can.
(dirvish-emerge-define-predicate executables
  "Matches executables."
  (eq ?x (aref (file-attribute-modes attrs) 3)))

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
                                    (directory-files default-directory))))
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

(defun dirvish-emerge--update-groups (groups)
  "Update dir-local groups to GROUPS."
  (setq-local dirvish-emerge-groups groups)
  (setf (alist-get 'dirvish-emerge-groups
                   (alist-get
                    'dirvish-mode
                    (alist-get (expand-file-name default-directory)
                               dir-locals-class-alist nil nil #'string=)))
        groups))

(defun dirvish-emerge--create-infix
    (ifx description recipe &optional selected hide)
  "Create an transient infix IFX of emerge group.
DESCRIPTION, RECIPE, SELECTED and HIDE are inserted into the
corresponding slots."
  (eval `(transient-define-infix ,ifx ()
           :class 'dirvish-emerge-group
           :recipe ',recipe
           :selected ,selected
           :hide ,hide
           :description ,description)))

(defun dirvish-emerge--create-infixes ()
  "Define and collect emerge groups from `dirvish-emerge-groups'."
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
         (groups (cl-loop for o in ifxes
                          collect (list (oref o description) (oref o recipe)
                                        (oref o hide) (oref o selected)))))
    (dirvish-emerge-mode 1)
    (revert-buffer)
    (dirvish-prop :force-emerge t)
    (setq-local dirvish-emerge-groups groups)))

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
  (let ((type (pcase (read-char-choice
                      "Press e for extensions, p for predicate, r for regex: "
                      '(?e ?p ?r))
                (101 'extensions) (112 'predicate) ('114 'regex)))
        (names (mapcar #'car dirvish-emerge-groups))
        (groups (buffer-local-value 'dirvish-emerge-groups (current-buffer)))
        (idx 1) (default "Anon-1") recipe title)
    (while (member default names)
      (cl-incf idx)
      (setq default (format "Anon-%s" idx)))
    (setq recipe (dirvish-emerge-read-recipe (cons type nil)))
    (setq title (read-string "Group title: " default))
    (push (list title (cons type recipe)) groups)
    (dirvish-emerge--update-groups groups)
    (dirvish-emerge-menu)))

(defun dirvish-emerge--ifx-remove ()
  "Remove an emerge group from `transient-current-suffixes'."
  (cl-loop for obj in transient-current-suffixes
           when (and (eq (type-of obj) 'dirvish-emerge-group)
                     (oref obj selected))
           do (dirvish-emerge--update-groups
               (assoc-delete-all (oref obj description)
                                 dirvish-emerge-groups #'equal)))
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
           finally
           (let* ((sel (cl-loop for o in (reverse sel) collect
                                (list (oref o description) (oref o recipe)
                                      (oref o hide) (oref o selected))))
                  (groups (if demote (append dirvish-emerge-groups sel)
                                (append sel dirvish-emerge-groups))))
             (dirvish-emerge--update-groups groups)))
  (dirvish-emerge-menu))

(defun dirvish-emerge--ifx-read ()
  "Read groups from .dir-locals.el."
  (dirvish-emerge--readin-groups-1 t)
  (dirvish-emerge-menu))

(defun dirvish-emerge--ifx-write ()
  "Write groups to .dir-locals.el."
  (add-dir-local-variable
   'dired-mode 'dirvish-emerge-groups
   (cl-loop for o in transient-current-suffixes
            when (eq (type-of o) 'dirvish-emerge-group) collect
            (list (oref o description) (oref o recipe)
                  (oref o hide) (oref o selected)))))

(defun dirvish-emerge--readin-groups-1 (&optional re-read)
  "Helper for `dirvish-emerge--readin-groups'.
When RE-READ, read groups from .dir-locals.el regardless of cache."
  (let ((dir-locals-directory-cache
         (if re-read nil dir-locals-directory-cache)))
    (hack-dir-local-variables))
  (let* ((dir-local (cdr (assq 'dirvish-emerge-groups
                               file-local-variables-alist)))
         (groups
          (cond (re-read dir-local)
                ((local-variable-if-set-p 'dirvish-emerge-groups)
                 (buffer-local-value 'dirvish-emerge-groups (current-buffer)))
                (dir-local dir-local)
                (t (default-value 'dirvish-emerge-groups)))))
    (hack-one-local-variable 'dirvish-emerge-groups groups)
    (dirvish-prop :emerge-preds
      (cl-loop for idx from 0 to (1- (length groups))
               for (_desc recipe) in groups collect
               (cons idx (dirvish-emerge--make-pred recipe))))))

(defun dirvish-emerge--readin-groups (&optional _dv _entry buffer)
  "Readin emerge groups in BUFFER for session DV."
  (with-current-buffer (or buffer (current-buffer))
    (dirvish-emerge--readin-groups-1)))

(defvar dirvish-emerge-group-heading-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'dirvish-emerge-toggle-current-group)
    map)
  "Keymap used when over a group heading.")

(defun dirvish-emerge--group-heading (desc hide)
  "Format emerge group heading in Dirvish buffer.
DESC and HIDE are the group title and visibility respectively."
  (let ((prefix (propertize " " 'font-lock-face
                            '(:inherit dirvish-emerge-group-title
                                       :strike-through t)))
        (title (propertize (format " %s%s " desc (if hide " (Hidden)" ""))
                           'font-lock-face 'dirvish-emerge-group-title))
        (suffix (propertize " " 'display '(space :align-to right)
                            'font-lock-face
                            '(:inherit dirvish-emerge-group-title
                                       :strike-through t))))
    (propertize (format "%s%s%s\n" prefix title suffix)
                'keymap dirvish-emerge-group-heading-map)))

(defun dirvish-emerge--insert-group (group)
  "Insert an individual GROUP to buffer."
  (pcase-let* ((`(,idx ,desc ,hide ,files) group)
               (beg (point)) (empty nil))
    (when (listp files)
      (setq empty (not files)
            files (mapconcat #'concat (nreverse files) "")))
    (unless empty (insert (dirvish-emerge--group-heading desc hide)))
    (unless hide (insert files))
    (let ((o (make-overlay beg (point))))
      (overlay-put o 'evaporate t)
      (overlay-put o 'dirvish-emerge
                   (list idx desc hide (unless empty files) empty))
      (push o dirvish-emerge--group-overlays))))

(defun dirvish-emerge--insert-groups (groups &optional pos beg end)
  "Insert GROUPS then resume cursor to POS.
POS can be a integer or filename.
BEG and END determine the boundary of groups."
  (unless (or beg end)
    (setq beg (dirvish-prop :content-begin)
          end (- (dired-subdir-max) (if (cdr dired-subdir-alist) 1 0))))
  (with-silent-modifications
    (setq dirvish-emerge--group-overlays nil)
    (delete-region beg end)
    (mapc #'dirvish-emerge--insert-group groups)
    (setq dirvish-emerge--group-overlays
          (nreverse dirvish-emerge--group-overlays)))
  (cond ((numberp pos) (goto-char pos))
        ((stringp pos) (dired-goto-file pos))))

(defun dirvish-emerge--apply-1 (preds)
  "Helper for `dirvish-emerge--apply'.
PREDS are locally composed predicates."
  (let ((old-file (dirvish-prop :index))
        (groups (cl-loop
                 with grs = (append dirvish-emerge-groups
                                    '(("-" nil nil)))
                 for i from 0
                 for (desc _ hide) in grs
                 collect (list i desc hide '())))
        (beg (progn (goto-char (point-min)) (dirvish-prop :content-begin)))
        (end (- (dired-subdir-max) (if (cdr dired-subdir-alist) 1 0)))
        (max-idx (length preds))
        (dir (file-local-name (dired-current-directory))))
    (while (< (point) end)
      (when-let ((f-beg (dired-move-to-filename))
                 (f-end (dired-move-to-end-of-filename)))
        (let* ((l-beg (line-beginning-position))
               (l-end (1+ (line-end-position)))
               (local (buffer-substring-no-properties f-beg f-end))
               (full (concat dir local))
               (type (dirvish-attribute-cache full :type))
               (attrs (dirvish-attribute-cache full :builtin))
               (match (cl-loop for (index . fn) in preds
                               for match = (funcall fn local full type attrs)
                               thereis (and match index))))
          (push (buffer-substring-no-properties l-beg l-end)
                (nth 3 (nth (or match max-idx) groups)))))
      (forward-line 1))
    (dirvish-emerge--insert-groups groups old-file beg end)))

(defun dirvish-emerge--apply ()
  "Readin `dirvish-emerge-groups' and apply them."
  (when (and (not (dirvish-prop :fd-arglist))
             (or (dirvish-prop :force-emerge)
                 (< (hash-table-count dirvish--attrs-hash)
                    dirvish-emerge-max-file-count)))
    (dirvish-emerge--readin-groups)
    (when-let ((preds (dirvish-prop :emerge-preds)))
      (dirvish-emerge--apply-1 preds))))

;;;; Interactive commands

;;;###autoload
(defun dirvish-emerge-menu ()
  "Manage pinned files in Dirvish."
  (interactive)
  (dirvish-emerge--readin-groups)
  (eval
   `(transient-define-prefix dirvish-emerge--menu ()
      "Manage pinned files in Dirvish."
      [:description
       (lambda () (dirvish--format-menu-heading
              "Manage Emerging Groups"
              "Press the index (like \"1\") to select the group
Press again to set the value for the group"))
       ["Active groups:"
        ,@(if dirvish-emerge-groups
              (dirvish-emerge--create-infixes)
            (list '("+" "  Press + to add a group"
                    (lambda () (interactive) (dirvish-emerge--ifx-add)))))]
       ["Actions:"
        ("RET" "Apply current setup" (lambda () (interactive) (dirvish-emerge--ifx-apply)))
        ("u" "  Unselect all groups"
         (lambda () (interactive) (dirvish-emerge--ifx-unselect)) :transient t)
        ("v" "  Toggle visibility of selected"
         (lambda () (interactive) (dirvish-emerge--ifx-toggle-hiding)) :transient t)
        ("a" "  Add a group"
         (lambda () (interactive) (dirvish-emerge--ifx-add)))
        ("x" "  Remove selected groups"
         (lambda () (interactive) (dirvish-emerge--ifx-remove)))
        ("t" "  Promote selected groups (top)"
         (lambda () (interactive) (dirvish-emerge--ifx-promote)))
        ("b" "  Demote selected groups (bottom)"
         (lambda () (interactive) (dirvish-emerge--ifx-promote 'demote)))
        ("n" "  Jump to next group" dirvish-emerge-next-group
         :transient t :if (lambda () dirvish-emerge--group-overlays))
        ("p" "  Jump to previous group" dirvish-emerge-previous-group
         :transient t :if (lambda () dirvish-emerge--group-overlays))
        ("r" "  Read groups from .dir-locals.el"
         (lambda () (interactive) (dirvish-emerge--ifx-read)))
        ("w" "  Write groups to .dir-locals.el"
         (lambda () (interactive) (dirvish-emerge--ifx-write)))]]))
  (dirvish-emerge--menu))

;;;###autoload
(define-minor-mode dirvish-emerge-mode
  "Toggle grouping of files in Dirvish."
  :group 'dirvish
  (if dirvish-emerge-mode
      (progn
        (add-hook 'dirvish-setup-hook #'dirvish-emerge--apply nil t)
        (unless dirvish-emerge--group-overlays (dirvish-emerge--apply)))
    (remove-hook 'dirvish-setup-hook #'dirvish-emerge--apply t)
    (mapc #'delete-overlay dirvish-emerge--group-overlays)
    (setq dirvish-emerge--group-overlays nil)
    (revert-buffer)))

(defun dirvish-emerge--get-group-overlay ()
  "Return overlay for the group at point."
  (unless dirvish-emerge--group-overlays
    (user-error "Dirvish: no groups applied here"))
  (let ((pos (point)))
    (cl-find-if (lambda (o) (and (overlay-start o)
                            (< pos (overlay-end o))
                            (>= pos (overlay-start o))))
                dirvish-emerge--group-overlays)))

(defun dirvish-emerge-next-group (arg)
  "Jump to the first file in the next ARG visible group."
  (interactive "^p")
  (let* ((old-ov (dirvish-emerge--get-group-overlay))
         (old-idx (cl-position old-ov dirvish-emerge--group-overlays))
         (target (+ old-idx arg))
         (len (1- (length dirvish-emerge--group-overlays)))
         (idx (max (min len target) 0))
         (target-ov (nth idx dirvish-emerge--group-overlays)))
    (while (and (not (or (>= idx len) (<= idx 0)))
                (not (overlay-start target-ov)))
      (setq idx (max (min len (+ idx (if (> arg 0) 1 -1))) 0))
      (setq target-ov (nth idx dirvish-emerge--group-overlays)))
    (cond ((eq old-idx idx))
          ((and target-ov (overlay-start target-ov))
           (goto-char (overlay-start target-ov))))))

(defun dirvish-emerge-previous-group (arg)
  "Jump to the first file in the previous ARG visible group."
  (interactive "^p")
  (dirvish-emerge-next-group (- 0 arg)))

(defun dirvish-emerge-toggle-current-group ()
  "Toggle the current group."
  (interactive)
  (cl-loop
   with curr-ov = (dirvish-emerge--get-group-overlay)
   with groups = ()
   with pos = (if (dirvish-prop :index) (overlay-start curr-ov) (point))
   for o in dirvish-emerge--group-overlays
   for (idx desc hide files) = (overlay-get o 'dirvish-emerge)
   do (when (eq curr-ov o)
        (setq hide (not hide))
        (let ((group (nth idx dirvish-emerge-groups)))
          (if (< (length group) 3)
              (cl-callf append group '(t))
            (cl-callf not (nth 2 group))))
        (when hide
          (setq files (buffer-substring
                       (save-excursion (goto-char (overlay-start o))
                                       (forward-line 1) (point))
                       (overlay-end o)))))
   do (push (list idx desc hide files) groups)
   finally (dirvish-emerge--insert-groups (nreverse groups) pos)))

(provide 'dirvish-emerge)
;;; dirvish-emerge.el ends here
