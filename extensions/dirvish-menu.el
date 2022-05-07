;;; dirvish-menu.el --- Keybindings menu for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.2.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "1.2.0") (transient "3.6.7"))

;;; Commentary:

;;; This library is a Dirvish extension which provides transient-based help
;;; menu.  Most commands in the menu are just built-in Dired commands.

;;; Code:

(unless (require 'transient nil t)
  (user-error "Dirvish: help menu requires `transient.el'"))

(require 'subr-x)
(require 'transient)
(require 'dirvish)
(defvar dirvish-menu-available-prefixs
  '(dirvish-setup-menu dirvish-ls-switches-menu dirvish-quicksort dirvish-yank-menu dirvish-goto-bookmark))

(defclass dirvish-menu:toggle (transient-infix)
  ((variable  :initarg :variable)
   (scope     :initarg :scope nil))
  "[Experimental] Class for Dirvish toggles.")

(cl-defmethod transient-format-description ((obj dirvish-menu:toggle))
  (or (oref obj description) (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj dirvish-menu:toggle))
  (let* ((val (prin1-to-string (oref obj value)))
         (face (if (equal val "On") 'transient-argument 'transient-inactive-value)))
    (propertize val 'face face)))

(cl-defmethod transient-init-value ((obj dirvish-menu:toggle))
  (oset obj value (if (memq (oref obj variable) (funcall (oref obj scope) (dirvish-curr))) 'On 'Off)))

(cl-defmethod transient-infix-read ((obj dirvish-menu:toggle))
  (oset obj value (if (eq (oref obj value) 'On) 'Off 'On)))

(cl-defmethod transient-infix-set ((obj dirvish-menu:toggle) value)
  (let* ((dv (dirvish-curr))
         (item (oref obj variable))
         (slot-name (oref obj scope))
         (curr-val (funcall slot-name dv))
         (new-val (if (eq value 'On) (push item curr-val) (remq item curr-val))))
    (cl-case slot-name
      ('dv-attributes (setf (dv-attributes dv) new-val))
      ('dv-preview-dispatchers (setf (dv-preview-dispatchers dv) new-val)))
    (dirvish--refresh-slots dv)
    (revert-buffer)))

(defun dirvish-menu--format-heading (string &optional center scale)
  "Format STRING as a menu heading.
When CENTER, align it at center.  SCALE defaults to 1.2."
  (setq scale (or scale 1.2))
  (concat
   (when center (propertize " " 'display `(space :align-to (- center ,(floor (/ (length string) scale))))))
   (propertize (capitalize string)
               'face '(:inherit dired-mark :underline t)
               'display `((raise (/ (- scale 1) 2)) (height ,scale)))))

(defun dirvish--change-depth (level)
  "Change parent depth of current Dirvish to LEVEL."
  (let ((dv (dirvish-curr)))
    (setf (dv-depth dv) level)
    (setf (dv-fullscreen-depth dv) level)
    (dirvish-build dv)))

(defmacro dirvish-menu--transient-define-multi (spec)
  "Define transient command with core information from SPEC."
  `(prog1 'dirvish-menu
     ,@(mapcar
        (lambda (elm)
          (let* ((pkg  (pop elm))
                 (prefix (intern (format "dirvish-%s-menu" pkg)))
                 (args elm))
            `(progn
               (add-to-list 'dirvish-menu-available-prefixs ',prefix)
               (transient-define-prefix ,prefix ()
                 ,(format "Dirvish commands menu for `%s'." pkg)
                 ,@args))))
        spec)))

;;;###autoload (autoload 'dirvish-ls-switches-menu "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-mark-actions-menu "dirvish-menu" nil t)
(dirvish-menu--transient-define-multi
 ((top-level
   [:description
    (lambda () (dirvish-menu--format-heading (format "%s help menu" (if (derived-mode-p 'dirvish-mode) "Dirvish" "Dired")) t 1.3))
    :if-derived dired-mode
    ["Essential commands"
     ("e"   "Open file"                           dired-find-file)
     ("o"   "Open file other window"              dired-find-file-other-window)
     ("w"   "Get file information"                dirvish-file-info-menu)
     ("s"   "Setup listing switches"              dirvish-ls-switches-menu)
     ("v"   "View this file"                      dired-view-file)
     ("g"   "Refresh buffer"                      revert-buffer)]
    ["I/O commands"
     ("a"   "Add an empty file"                   dired-create-empty-file)
     ("C"   "Copy"                                dired-do-copy :if-mode dired-mode)
     ("C"   "Paste marked files"                  dirvish-yank :if-derived dirvish-mode)
     ("@"   "Rename files"                        dirvish-renaming-menu)
     ("X"   "Delete files"                        dired-do-delete)
     ("f"   "Edit file's attributes"              dirvish-file-attributes-menu)
     ("+"   "Create a directory"                  dired-create-directory)]
    ["View | Layout"
     ("(" "  Toggle details"                      dired-hide-details-mode)
     ("." "  Apply filters"                       dired-filter-mode :if (lambda () (featurep 'dired-filter)))
     ("." "  Apply filters"                       dired-omit-mode :if-not (lambda () (featurep 'dired-filter)))
     ("=" "  Compare files"                       dired-diff)
     ("S" "  Manage subdirs"                      dirvish-subdir-menu)
     ("M-f" "Toggle fullscreen"                   dirvish-toggle-fullscreen :if-derived dirvish-mode)
     ("M-s" "Setup Dirvish"                       dirvish-setup-menu :if-derived dirvish-mode)]
    ["Navigation"
     ("j" "  Jump to file line"                   dired-goto-file)
     ("b" "  Jump to bookmarks"                   dirvish-goto-bookmark)
     ("^" "  Go up directory"                     dired-up-directory)
     ("r" "  Roam the file system"                dirvish-roam)
     ("l" "  Goto the last place"                 dirvish-other-buffer)
     ("SPC" "Recently visited"                    dirvish-show-history)]
    ["Marks"
     ("m" "  Mark this file"                      dired-mark)
     ("u" "  Unmark this file"                    dired-unmark)
     ("U" "  Unmark all"                          dired-unmark-all-marks)
     ("t" "  Toggle marks"                        dired-toggle-marks)
     ("*" "  Mark by.."                           dirvish-marking-menu)
     ("M-a" "Actions on marks"                    dirvish-mark-actions-menu)]
    ["Extensions"
     (":" "  GnuPG helpers"                       dirvish-epa-dired-menu)
     ("TAB" "Toggle subtree"                      dired-subtree-toggle :if (lambda () (featurep 'dired-subtree)))
     ("M-c" "Collapse paths"                      dired-collapse-mode :if (lambda () (featurep 'dired-collapse)))
     ("N" "  Live narrowing"                      dired-narrow :if (lambda () (featurep 'dired-narrow)))]])
  (ls-switches
   :init-value
   (lambda (o) (oset o value (split-string (or dired-actual-switches ""))))
   [:description
    (lambda ()
      (let ((title "setup listing switches")
            (note "lowercased switches also work in"))
        (format "%s\n%s %s" (dirvish-menu--format-heading title)
                (propertize note 'face 'font-lock-doc-face)
                (propertize "dired-hide-details-mode" 'face 'font-lock-doc-markup-face))))
    ["options"
     ("a" dirvish-menu--ls-filter)
     ("s" dirvish-menu--ls-sort)
     ("i" dirvish-menu--ls-indicator-style)
     ("t" dirvish-menu--ls-time)
     ("T" dirvish-menu--ls-time-style)
     ("B" "scale sizes when printing, eg. 10K" "--block-size=")
     "toggles"
     ("r" "reverse order while sorting" "--reverse")
     ("d" "list directories ontop" "--group-directories-first")
     ("~" "hide backups files (eg. foo~)" "--ignore-backups")
     ("A" "show the author" "--author")
     ("C" "show security context" "--context")
     ("H" "human readable file size" "--human-readable")
     ("G" "hide group names" "--no-group")
     ("O" "hide owner names" "-g")
     ("L" "info for link references or link itself" "--dereference")
     ("N" "numeric user and group IDs" "--numeric-uid-gid")
     ("P" "powers of 1000 for file size rather than 1024" "--si")
     ("I" "show index number" "--inode")
     ("S" "show the allocated size" "--size")
     "Actions"
     ("RET" "  apply to this buffer" dirvish-menu--apply-switches-to-buffer)
     ("M-RET" "apply to this session" dirvish-menu--apply-switches-to-session)
     ("C-r" "  reset this buffer" dirvish-menu--reset-switches-for-buffer)
     ("M-r" "  reset this session" dirvish-menu--reset-switches-for-session)
     ("C-l" "  clear choices" dirvish-menu--clear-switches-choices :transient t)]])
  (file-attributes
   ["Change file attributes"
    ("R"   "Name"                                 dired-do-rename)
    ("G"   "Group"                                dired-do-chgrp)
    ("M"   "Mode"                                 dired-do-chmod)
    ("O"   "Owner"                                dired-do-chown)
    ("T"   "Timestamp"                            dired-do-touch)])
  (subdir
   ["Manage subdirs"
    ("i" "  Insert subdir"                        dired-maybe-insert-subdir)
    ("k" "  Kill subdir"                          dired-kill-subdir)
    ("n" "  Next subdir"                          dired-next-subdir)
    ("p" "  Prev subdir"                          dired-prev-subdir)
    ("j" "  Jump to subdir"                       dired-goto-subdir)
    ("$" "  Hide subdir"                          dired-hide-subdir)
    ("M-$" "Hide all subdirs"                     dired-hide-all)])
  (marking
   ["Mark all ____ files:"
    ("s"   "SUBDIR"                               dired-mark-subdir-files)
    ("*"   "EXECUTABLE"                           dired-mark-executables)
    ("/"   "DIRECTORIE"                           dired-mark-directories)
    ("@"   "SYMLINK"                              dired-mark-symlinks)
    ("e"   "EXTENSION"                            dired-mark-extension)
    ("&"   "GARBAGE"                              dired-flag-garbage-files)
    ("#"   "AUTO-SAVED"                           dired-flag-auto-save-files)
    ("~"   "BACKUP"                               dired-flag-backup-files)
    ("."   "NUMERICAL BACKUP"                     dired-clean-directory)
    ("%"   "NAME     matched regexp"              dired-mark-files-regexp)
    ("g"   "CONTENTS matched regexp"              dired-mark-files-containing-regexp)])
  (mark-actions
   ["Actions on marks"
    ("F"   "Open"                                 dired-do-find-marked-files)
    ("S"   "Symlink"                              dired-do-symlink)
    ("H"   "Hardlink"                             dired-do-hardlink)
    ("P"   "Print"                                dired-do-print)
    ("X"   "Delete flagged"                       dired-do-flagged-delete)
    ("A"   "Search file contents"                 dired-do-find-regexp)
    ("R"   "Replace file contents"                dired-do-find-regexp-and-replace)
    ("B"   "Byte compile elisp"                   dired-do-byte-compile)
    ("L"   "Load elisp"                           dired-do-load)
    ("z"   "Compress to"                          dired-do-compress-to)
    ("Z"   "Compress"                             dired-do-compress)
    ("!"   "Shell command"                        dired-do-shell-command)
    ("&"   "Async shell command"                  dired-do-async-shell-command)
    ("N"   "Echo number of marked files"          dired-number-of-marked-files)
    ("c"   "Change mark type"                     dired-change-marks)
    ("k"   "Kill lines"                           dired-do-kill-lines)
    ("n"   "Move to next marked"                  dired-next-marked-file)
    ("p"   "Move to prev marked"                  dired-prev-marked-file)])
  (file-info
   ["Get file information"
    ("n"   "copy file NAME"                       dirvish-copy-file-name)
    ("p"   "copy file PATH"                       dirvish-copy-file-path)
    ("d"   "copy file DIRECTORY"                  dirvish-copy-file-directory)
    ("l"   "copy symlink's truename"              dirvish-copy-file-true-path)
    ("L"   "goto symlink's truename"              dirvish-find-file-true-path)
    ("s"   "file size of marked files"            dirvish-total-file-size)
    ("t"   "show file TYPE"                       dired-show-file-type)])
  (renaming
   ["File renaming"
    ("u"   "Upper-case file name"                 dired-upcase)
    ("l"   "Lower-case file name"                 dired-downcase)
    ("_"   "Replace SPC with UNDERSCORE"          dirvish-rename-space-to-underscore)
    ("w"   "Enter wdired [writable dired]"        wdired-change-to-wdired-mode :if-not-derived wdired-mode)])
  (epa-dired
   ["GNUpg assistant"
    ("e"   "Encrypt"                              epa-dired-do-encrypt)
    ("d"   "Decrypt"                              epa-dired-do-decrypt)
    ("v"   "Verify"                               epa-dired-do-verify)
    ("s"   "Sign"                                 epa-dired-do-sign)])))

;;;###autoload
(defun dirvish-dispatch ()
  "Summon dirvish commands menu."
  (interactive)
  (let ((transient-display-buffer-action
         '(display-buffer-in-side-window
           (side . bottom)
           (dedicated . t)
           (inhibit-same-window . t)
           (window-parameters (no-other-window . t))))
        (transient-show-popup t))
    (dirvish-top-level-menu)))

(defun dirvish-menu--clear-switches-choices ()
  "Reload the listing switches setup UI."
  (interactive)
  (transient-setup 'dirvish-ls-switches-menu))

(defun dirvish-menu--apply-switches-to-buffer (&optional switches)
  "Apply listing SWITCHES to current buffer."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (switches (or switches (string-join (append '("-l") args) " "))))
    (setq dired-actual-switches switches)
    (revert-buffer)))

(defun dirvish-menu--apply-switches-to-session (&optional switches)
  "Apply listing SWITCHES to current session."
  (interactive)
  (let* ((dv (dirvish-curr))
         (args (transient-args transient-current-command))
         (switches (or switches (string-join (append '("-l") args) " "))))
    (dolist (buf (mapcar #'cdr (dv-root-dir-buf-alist dv)))
      (with-current-buffer buf
        (setq dired-actual-switches switches)
        (revert-buffer)))
    (setf (dv-ls-switches dv) switches)))

(defun dirvish-menu--reset-switches-for-buffer ()
  "Reset listing switches for current buffer."
  (interactive)
  (dirvish-menu--apply-switches-to-buffer dired-listing-switches))

(defun dirvish-menu--reset-switches-for-session ()
  "Reset listing switches for current buffer."
  (interactive)
  (dirvish-menu--apply-switches-to-session dired-listing-switches))

(transient-define-infix dirvish-menu--ls-filter ()
  :description "show all files"
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(all\\|almost-all\\)\\)"
  :choices '("all" "almost-all"))

(transient-define-infix dirvish-menu--ls-sort ()
  :description "sort by"
  :class 'transient-switches
  :argument-format "--sort=%s"
  :argument-regexp "\\(--sort=\\(time\\|none\\|extension\\|size\\|version\\|width\\)\\)"
  :choices '("time" "none" "extension" "size" "version" "width"))

(transient-define-infix dirvish-menu--ls-time ()
  :description "show time as | sort files with"
  :class 'transient-switches
  :argument-format "--time=%s"
  :argument-regexp "\\(--time=\\(use\\|birth\\|ctime\\)\\)"
  :choices '("use" "birth" "ctime"))

(transient-define-infix dirvish-menu--ls-time-style ()
  :description "time style"
  :class 'transient-switches
  :argument-format "--time-style=%s"
  :argument-regexp "\\(--time-style=\\(full-iso\\|long-iso\\|iso\\|locale\\|+\\)\\)"
  :choices '("full-iso" "long-iso" "iso" "locale" "+"))

(transient-define-infix dirvish-menu--ls-indicator-style ()
  :description "add indicator"
  :class 'transient-switches
  :argument-format "--indicator-style=%s"
  :argument-regexp "\\(--indicator-style=\\(slash\\|file-type\\|classify\\)\\)"
  :choices '("slash" "file-type" "classify"))

(defun dirvish-quicksort--do-sort (switches)
  "Sort current buffer with dired sort SWITCHES."
  (let* ((regexp "\\(--time=\\w+\\|--sort=\\w+\\|--reverse\\)\\( \\)?")
         (others (replace-regexp-in-string regexp "" dired-actual-switches))
         (new-switches (concat others " " switches)))
    (setq dired-actual-switches new-switches)
    (revert-buffer)))

(define-obsolete-function-alias 'dirvish-sort-by-criteria 'dirvish-quicksort "1.2.0")
;;;###autoload (autoload 'dirvish-quicksort "dirvish-menu" nil t)
(defcustom dirvish-quicksort-keys
  '(("n" ""                                   "name (a-z)")
    ("N" "--reverse"                          "name (z-a)")
    ("e" "--sort=extension"                   "extension (a-z)")
    ("E" "--sort=extension --reverse"         "extension (z-a)")
    ("s" "--sort=size"                        "size (largest first)")
    ("S" "--sort=size --reverse"              "size (smallest first)")
    ("v" "--sort=version"                     "version number (earliest first)")
    ("V" "--sort=version --reverse"           "version number (latest first)")
    ("w" "--sort=width"                       "width (shortest first)")
    ("W" "--sort=width --reverse"             "width (longest first)")
    ("m" "--sort=time"                        "modification time (newest first)")
    ("M" "--sort=time --reverse"              "modification time (oldest first)")
    ("a" "--sort=time --time=use"             "access time (newest first)")
    ("A" "--sort=time --time=use --reverse"   "access time (oldest first)")
    ("b" "--sort=time --time=birth"           "birth time (newest first)")
    ("B" "--sort=time --time=birth --reverse" "birth time (oldest first)")
    ("c" "--sort=time --time=ctime"           "change time (newest first)")
    ("C" "--sort=time --time=ctime --reverse" "change time (oldest first)"))
  "SORT-KEYs for command `dirvish-quicksort'.
A SORT-KEY is a (KEY SWITCHES DOC) alist where KEY is the key to
invoke the sort function, SWITCHES is the the sort flags for
`dired-sort-other', DOC is the documentation string."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (eval
     `(transient-define-prefix dirvish-quicksort ()
        "Sort Dirvish buffer by different criteria."
        ["Sort by: "
         ,@(cl-loop
            for (key switches desc) in v collect
            (list key desc `(lambda ()
                              (interactive)
                              (dirvish-quicksort--do-sort ,switches))))]))))

;;;###autoload (autoload 'dirvish-yank-menu "dirvish-menu" nil t)
(defcustom dirvish-yank-keys
  '(("y" "Yank (paste) here"           dirvish-yank)
    ("m" "Move here"                   dirvish-move)
    ("s" "Make symlinks here"          dirvish-symlink)
    ("r" "Make relative symlinks here" dirvish-relative-symlink)
    ("h" "Make hardlinks here"         dirvish-hardlink))
  "YANK-KEYs for command `dirvish-yank-menu'.
A YANK-KEY is a (KEY DOC CMD) alist where KEY is the key to
invoke the CMD, DOC is the documentation string."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (eval
     `(transient-define-prefix dirvish-yank-menu ()
        "Yank commands menu."
        ["Select yank operation on marked files:" ,@v]))))

;;;###autoload (autoload 'dirvish-goto-bookmark "dirvish-menu" nil t)
(defcustom dirvish-bookmarks-alist
  '(("h" "~/"                          "Home")
    ("d" "~/Downloads/"                "Downloads")
    ("m" "/mnt/"                       "Drives")
    ("t" "~/.local/share/Trash/files/" "TrashCan"))
  "BOOKMARKs for command `dirvish-goto-bookmark'.
A BOOKMARK is a (KEY PATH DOC) alist where KEY is the key to
invoke the navigation, PATH is the the argument for command
`dirvish-find-file', DOC (optional) is the documentation string."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (let ((max-desc-len (seq-max (mapcar (lambda (i) (length (nth 2 i))) v))))
      (eval
       `(transient-define-prefix dirvish-goto-bookmark ()
          "Open frequently visited directories using dirvish."
          ["Go to Directory: "
           ,@(cl-loop
              for (key path desc) in v
              collect
              (list key
                    (concat desc "  "
                            (make-string (- max-desc-len (length desc)) ?\ )
                            (propertize path 'face 'font-lock-comment-face))
                    `(lambda () (interactive) (dirvish-find-file ,path))))])))))

(define-obsolete-function-alias 'dirvish-ui-config 'dirvish-setup-menu "1.0.0")
(defconst dirvish--icon-backend (cond ((memq 'all-the-icons dirvish-attributes) 'all-the-icons)
                                      ((memq 'vscode-icon dirvish-attributes) 'vscode-icon)))
;;;###autoload (autoload 'dirvish-setup-menu "dirvish-menu" nil t)
(defcustom dirvish-setup-menu-alist
  `(,(when dirvish--icon-backend `("i" ,dirvish--icon-backend attr "File icons"))
    ("s" file-size      attr    "File size (right-aligned)")
    ("g" vc-state       attr    "VC state (left-aligned)")
    ("m" git-msg        attr    "Git commit messages")
    ("e" expanded-state attr    "Node expanded state")
    ("d" vc-diff        preview "VC diff")
    ("0" 0              column  "main column only")
    ("1" 1              column  "main + 1 parent (ranger like)")
    ("2" 2              column  "main + 2 parent")
    ("3" 3              column  "main + 3 parent")
    ("4" 4              column  "main + 4 parent"))
  "TOGGLEs for `dirvish-setup-menu'.
A TOGGLE is a list consists of (KEY VAR SCOPE DESCRIPTION) where
KEY is a string passed to `kbd', VAR is a valid attribute (as in
`dirvish-attributes') or preview dispatcher (as in
`dirvish-preview-dispatchers'), SCOPE is either 'attributes or
'preview-dps, DESCRIPTION is a optional description for the VAR."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k (remove nil v))
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attr)) v))
          (preview-alist (seq-filter (lambda (i) (eq (nth 2 i) 'preview)) v))
          (column-alist (seq-filter (lambda (i) (eq (nth 2 i) 'column)) v)))
      (cl-labels ((new-infix (i)
                    (let* ((infix-var (nth 1 i))
                           (infix-name (intern (format "dirvish-%s-infix" (nth 1 i))))
                           (slot-name (pcase (nth 2 i)
                                        ('attr "attributes")
                                        ('preview "preview-dispatchers")))
                           (infix-scope (intern (format "dv-%s" slot-name)))
                           (infix-desc (nth 3 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish-menu:toggle
                               :variable ',infix-var
                               :scope ',infix-scope
                               :description ,infix-desc))))
                  (expand-infix (i) (list (car i) (intern (format "dirvish-%s-infix" (nth 1 i)))))
                  (column-option (i) (list (car i) (nth 3 i)
                                           `(lambda () (interactive) (dirvish--change-depth ,(nth 1 i))))))
        (mapc #'new-infix attr-alist)
        (mapc #'new-infix preview-alist)
        (eval
         `(transient-define-prefix dirvish-setup-menu ()
            "Change config of current Dirvish session."
            [:description
             (lambda () (dirvish-menu--format-heading "Setup Dirvish"))
             ["File attributes:"
              ,@(mapcar #'expand-infix attr-alist)]]
            ["Preview:" :if-not dirvish-dired-p
             ,@(mapcar #'expand-infix preview-alist)]
            ["Layout:" :if (lambda () (and (derived-mode-p 'dirvish-mode) (not (dirvish-dired-p))))
             ,@(mapcar #'column-option column-alist)]
            ["Actions:"
             ("RET" "Confirm and quit" (lambda () (interactive) (dirvish-build (dirvish-curr)) (revert-buffer)))]))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here
