;;; dirvish-menu.el --- Transient-based help menu for Dired/Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.3.21
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "25.1") (transient "0.3.7"))

;;; Commentary:

;;; This library is a Dirvish extension which provides transient-based help
;;; menu.  Most commands in the menu are just built-in Dired commands.

;;; Code:

(declare-function dirvish-curr "dirvish")
(declare-function dirvish--refresh-slots "dirvish")
(declare-function dv-root-dir-buf-alist "dirvish")
(require 'transient)
(require 'dired)
(require 'dirvish nil t)

(defvar dirvish-menu-available-prefixs
  '(dirvish-setup-menu dirvish-ls-switches-menu dirvish-quicksort dirvish-yank-menu dirvish-goto-bookmark dirvish-mark-menu))

(defclass dirvish-menu-toggles (transient-infix)
  ((variable  :initarg :variable)
   (scope     :initarg :scope nil))
  "[Experimental] Class for Dirvish toggles.")

(cl-defmethod transient-format-description ((obj dirvish-menu-toggles))
  "Format description for DIRVISH-MENU-TOGGLES instance OBJ."
  (or (oref obj description) (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj dirvish-menu-toggles))
  "Format value for DIRVISH-MENU-TOGGLES instance OBJ."
  (let* ((val (prin1-to-string (oref obj value)))
         (face (if (equal val "On") 'transient-argument 'transient-inactive-value)))
    (propertize val 'face face)))

(cl-defmethod transient-init-value ((obj dirvish-menu-toggles))
  "Initialize value for DIRVISH-MENU-TOGGLES instance OBJ."
  (oset obj value (if (memq (oref obj variable) (funcall (oref obj scope) (dirvish-curr))) 'On 'Off)))

(cl-defmethod transient-infix-read ((obj dirvish-menu-toggles))
  "Read value from DIRVISH-MENU-TOGGLES instance OBJ."
  (oset obj value (if (eq (oref obj value) 'On) 'Off 'On)))

(cl-defmethod transient-infix-set ((obj dirvish-menu-toggles) value)
  "Set relevant value in DIRVISH-MENU-TOGGLES instance OBJ to VALUE."
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

(defun dirvish-menu--format-heading (string &optional scale)
  "Format STRING as a menu heading.
When CENTER, align it at center.  SCALE defaults to 1.2."
  (setq scale (or scale 1.2))
  (propertize (capitalize string)
              'face '(:inherit dired-mark :underline t)
              'display `((raise (/ (- scale 1) 2)) (height ,scale))))

(defmacro dirvish-menu--transient-define-multi (spec)
  "Define transient command with core information from SPEC."
  `(prog1 'dirvish-menu
     ,@(mapcar
        (lambda (elm)
          (let* ((name  (pop elm))
                 (suffix (if (eq name 'dispatch) "" "-menu"))
                 (cmd (intern (format "dirvish-%s%s" name suffix)))
                 (args elm))
            `(progn
               (add-to-list 'dirvish-menu-available-prefixs ',cmd)
               (transient-define-prefix ,cmd ()
                 ,@args))))
        spec)))

;;;###autoload (autoload 'dirvish-dispatch "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-ls-switches-menu "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-mark-menu "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-filter-menu "dirvish-menu" nil t)
(dirvish-menu--transient-define-multi
 ((dispatch
   "Main help menu for Dired/Dirvish."
   [:description
    (lambda () (dirvish-menu--format-heading (format "%s help menu" (if (derived-mode-p 'dirvish-mode) "Dirvish" "Dired")) 1.3))
    :if-derived dired-mode
    ["Essential commands"
     ("e" "  Open file"                           dired-find-file)
     ("o" "  Open file other window"              dired-find-file-other-window)
     ("w" "  Get file information"                dirvish-file-info-menu)
     ("l" "  Setup listing switches"              dirvish-ls-switches-menu)
     ("s" "  Sort current buffer"                 dirvish-quicksort)
     ("TAB" "Toggle subtree"                      dirvish-toggle-subtree :if-not (lambda () (featurep 'dired-subtree)))
     ("TAB" "Toggle subtree"                      dired-subtree-toggle :if (lambda () (featurep 'dired-subtree)))
     ("M-f" "Toggle fullscreen"                   dirvish-toggle-fullscreen :if-derived dirvish-mode)
     ("M-s" "Setup Dirvish"                       dirvish-setup-menu :if-derived dirvish-mode)]
    ["File operations"
     ("a" "  Add an empty file"                   dired-create-empty-file)
     ("+" "  Add a directory"                     dired-create-directory)
     ("@" "  Rename files"                        dirvish-renaming-menu)
     ("X" "  Delete files"                        dired-do-delete)
     ("v" "  View this file"                      dired-view-file)
     ("C" "  Copy"                                dired-do-copy :if-mode dired-mode)
     ("y" "  Yank marked files"                   dirvish-yank-menu :if-derived dirvish-mode)
     ("." "  Filter by.."                         dirvish-filter-menu :if (lambda () (featurep 'dired-filter)))
     ("." "  Toggle file omitting"                dired-omit-mode :if-not (lambda () (featurep 'dired-filter)))
     ("*" "  Actions on marked"                   dirvish-mark-menu)]]
   [["Navigation"
     ("j" "  Jump to line for file"               dired-goto-file)
     ("b" "  Go to bookmarks"                     dirvish-goto-bookmark)
     ("^" "  Go to parent directory"              dired-up-directory)
     ("r" "  Roam the file system"                dirvish-roam :if (lambda () (featurep 'dirvish)))
     ("m" "  Go to the MRU buffer"                dirvish-other-buffer :if (lambda () (featurep 'dirvish)))
     ("n" "  Forward history"                     dirvish-go-forward-history :transient t)
     ("p" "  Backward history"                    dirvish-go-backward-history :transient t)
     ("SPC" "Recently visited"                    dirvish-show-history :if (lambda () (featurep 'dirvish)))]
    ["Others"
     ("g" "  Refresh buffer"                      revert-buffer)
     ("S" "  Manage subdirs"                      dirvish-subdir-menu)
     ("(" "  Toggle details"                      dired-hide-details-mode)
     ("=" "  Compare files"                       dired-diff)
     (":" "  GnuPG helpers"                       dirvish-epa-dired-menu)
     ("M-c" "Collapse paths"                      dired-collapse-mode :if (lambda () (featurep 'dired-collapse)))
     ("N" "  Live narrowing"                      dired-narrow :if (lambda () (featurep 'dired-narrow)))]])
  (ls-switches
   "Setup Dired listing switches."
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
     ("M-RET" "apply to this session" dirvish-menu--apply-switches-to-session :if-derived 'dirvish-mode)
     ("C-r" "  reset this buffer" dirvish-menu--reset-switches-for-buffer)
     ("M-r" "  reset this session" dirvish-menu--reset-switches-for-session :if-derived 'dirvish-mode)
     ("C-l" "  clear choices" dirvish-menu--clear-switches-choices :transient t)]])
  (subdir
   "Help Menu for Dired subdir management."
   ["Manage subdirs"
    ("i" "  Insert subdir"                        dired-maybe-insert-subdir :transient t)
    ("k" "  Kill subdir"                          dired-kill-subdir :transient t)
    ("n" "  Next subdir"                          dired-next-subdir :transient t)
    ("p" "  Prev subdir"                          dired-prev-subdir :transient t)
    ("j" "  Jump to subdir"                       dired-goto-subdir)
    ("$" "  Hide subdir"                          dired-hide-subdir :transient t)
    ("M-$" "Hide all subdirs"                     dired-hide-all)])
  (mark
   "Help Menu for `dired-mark-*' commands."
   [["Mark or unmark files:"
     ("e"   "by Extension"                        dired-mark-extension :transient t)
     ("%"   "by Regexp (file name)"               dired-mark-files-regexp :transient t)
     ("g"   "by Regexp (file content)"            dired-mark-files-containing-regexp :transient t)
     ("s"   "by Subdir"                           dired-mark-subdir-files :transient t)
     ("*"   "by Executable"                       dired-mark-executables :transient t)
     ("/"   "by Directory"                        dired-mark-directories :transient t)
     ("@"   "by Symlink"                          dired-mark-symlinks :transient t)
     ("&"   "by Garbage"                          dired-flag-garbage-files :transient t)
     ("#"   "by Auto-saved"                       dired-flag-auto-save-files :transient t)
     ("~"   "by Backup"                           dired-flag-backup-files :transient t)
     ("."   "by Numerical backup"                 dired-clean-directory :transient t)
     ("u" "  Unmark this file"                    dired-unmark :transient t)
     ("DEL" "Unmark and move up line"             dired-unmark-backward :transient t)
     ("U" "  Unmark all files"                    dired-unmark-all-files :transient t)
     ("t" "  Toggle marks"                        dired-toggle-marks :transient t)
     ("C-n" "Move to next marked file"            dired-next-marked-file :transient t)
     ("C-p" "Move to prev marked file"            dired-prev-marked-file :transient t)]
    ["Actions on marked files:"
     ("F"   "Open"                                dired-do-find-marked-files)
     ("S"   "Symlink"                             dired-do-symlink)
     ("H"   "Hardlink"                            dired-do-hardlink)
     ("P"   "Print"                               dired-do-print)
     ("X"   "Delete flagged"                      dired-do-flagged-delete)
     ("r"   "Search file contents"                dired-do-find-regexp)
     ("R"   "Replace file contents"               dired-do-find-regexp-and-replace)
     ("B"   "Byte compile elisp"                  dired-do-byte-compile)
     ("L"   "Load elisp"                          dired-do-load)
     ("z"   "Compress to"                         dired-do-compress-to)
     ("Z"   "Compress"                            dired-do-compress)
     ("!"   "Shell command"                       dired-do-shell-command)
     ("&"   "Async shell command"                 dired-do-async-shell-command)
     ("N"   "Echo number of marked files"         dired-number-of-marked-files)
     ("l"   "Redisplay all marked files"          dired-do-redisplay)
     ("c"   "Change mark type"                    dired-change-marks)
     ("k"   "Kill lines"                          dired-do-kill-lines)]]
    ["Change file attributes:"
     ("R"   "Change file's NAME"                  dired-do-rename)
     ("G"   "Change file's GROUP"                 dired-do-chgrp)
     ("M"   "Change file's MODE"                  dired-do-chmod)
     ("O"   "Change file's OWNER"                 dired-do-chown)
     ("T"   "Change file's TIMESTAMP"             dired-do-touch)])
  (file-info
   "Gather file information."
   ["Get file information"
    ("n"   "Copy file NAME"                       dirvish-copy-file-name :if (lambda () (featurep 'dirvish)))
    ("N"   "Copy NAMEs of marked files"           dired-copy-filename-as-kill)
    ("p"   "Copy file PATH"                       dirvish-copy-file-path :if (lambda () (featurep 'dirvish)))
    ("d"   "Copy file DIRECTORY"                  dirvish-copy-file-directory :if (lambda () (featurep 'dirvish)))
    ("l"   "Copy symlink's truename"              dirvish-copy-file-true-path :if (lambda () (featurep 'dirvish)))
    ("L"   "Go to symlink's truename"             dirvish-find-file-true-path :if (lambda () (featurep 'dirvish)))
    ("s"   "Get total size of marked files"       dirvish-total-file-size :if (lambda () (featurep 'dirvish)))
    ("t"   "show file TYPE"                       dired-show-file-type)])
  (renaming
   "Help Menu for file renaming in Dired."
   ["File renaming"
    ("u"   "Upper-case file name"                 dired-upcase)
    ("l"   "Lower-case file name"                 dired-downcase)
    ("_"   "Replace SPC with UNDERSCORE"          dirvish-rename-space-to-underscore :if-derived 'dirvish-mode)
    ("w"   "Enter wdired [writable dired]"        wdired-change-to-wdired-mode :if-not-derived wdired-mode)])
  (filter
   "Transient-based `dired-filter-map'."
   ["Filter by:"
    ("n" "  Name"                                 dired-filter-by-name)
    ("r" "  Regexp"                               dired-filter-by-regexp)
    ("." "  Extension"                            dired-filter-by-extension)
    ("h" "  Dotfiles"                             dired-filter-by-dot-files)
    ("o" "  Omit"                                 dired-filter-by-omit)
    ("g" "  Garbage"                              dired-filter-by-garbage)
    ("e" "  Predicate"                            dired-filter-by-predicate)
    ("f" "  File"                                 dired-filter-by-file)
    ("d" "  Directory"                            dired-filter-by-directory)
    ("m" "  Mode"                                 dired-filter-by-mode)
    ("s" "  Symlink"                              dired-filter-by-symlink)
    ("x" "  Executable"                           dired-filter-by-executable)
    ("i" "  Git ignored"                          dired-filter-by-git-ignored)
    ""
    "Compose filters:"
    ("|" "  Or"                                   dired-filter-or)
    ("!" "  Negate"                               dired-filter-negate)
    ("*" "  Decompose"                            dired-filter-decompose)
    ("Tab" "Transpose"                            dired-filter-transpose)
    ("p" "  Pop"                                  dired-filter-pop)
    ("/" "  Pop all"                              dired-filter-pop-all)
    ("S" "  Save"                                 dired-filter-save-filters)
    ("D" "  Delete saved"                         dired-filter-delete-saved-filters)
    ("A" "  Add saved"                            dired-filter-add-saved-filters)
    ("L" "  Load saved"                           dired-filter-load-saved-filters)])
  (epa-dired
   "Help menu for `epa-dired-do-*' commands."
   ["GNUpg assistant"
    ("e"   "Encrypt"                              epa-dired-do-encrypt)
    ("d"   "Decrypt"                              epa-dired-do-decrypt)
    ("v"   "Verify"                               epa-dired-do-verify)
    ("s"   "Sign"                                 epa-dired-do-sign)])))

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

(defun dirvish-menu--quicksort-do-sort (switches)
  "Sort current buffer with dired sort SWITCHES."
  (let* ((regexp "\\(--time=\\w+\\|--sort=\\w+\\|--reverse\\)\\( \\)?")
         (others (replace-regexp-in-string regexp "" dired-actual-switches))
         (new-switches (concat others " " switches)))
    (setq dired-actual-switches new-switches)
    (revert-buffer)))

(define-obsolete-variable-alias 'dirvish-quicksort-keys 'dirvish-menu-quicksort-keys "1.3.21")
;;;###autoload (autoload 'dirvish-quicksort "dirvish-menu" nil t)
(defcustom dirvish-menu-quicksort-keys
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
                              (dirvish-menu--quicksort-do-sort ,switches))))]))))

(define-obsolete-variable-alias 'dirvish-yank-keys 'dirvish-menu-yank-keys "1.3.21")
;;;###autoload (autoload 'dirvish-yank-menu "dirvish-menu" nil t)
(defcustom dirvish-menu-yank-keys
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
        [:if-derived 'dirvish-mode
                     "Select yank operation on marked files:" ,@v]
        (interactive)
        (if (derived-mode-p 'dirvish-mode)
            (transient-setup 'dirvish-yank-menu)
          (user-error "Not in a Dirvish buffer"))))))

(define-obsolete-variable-alias 'dirvish-bookmarks-alist 'dirvish-menu-bookmarks "1.3.21")
;;;###autoload (autoload 'dirvish-goto-bookmark "dirvish-menu" nil t)
(defcustom dirvish-menu-bookmarks
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
                    `(lambda ()
                       (interactive)
                       (dired-jump current-prefix-arg ,path))))])))))

;;;###autoload (autoload 'dirvish-setup-menu "dirvish-menu" nil t)
(defcustom dirvish-menu-setup-items
  '(("i"  all-the-icons  attr     "File icons")
    ("s"  file-size      attr     "File size")
    ("v"  vc-state       attr     "Version control state information")
    ("m"  git-msg        attr     "Git commit messages")
    ("e"  expanded-state attr     "Directory expanded state")
    ("d"  vc-diff        preview  "Version control diff in preview window")
    ("1" '(0 nil  0.4)   layout   "       | CURRENT | preview")
    ("2" '(0 nil  0.8)   layout   "       | current | PREVIEW")
    ("3" '(1 0.08 0.8)   layout   "parent | current | PREVIEW")
    ("4" '(1 0.1  0.6)   layout   "parent | current | preview"))
  "ITEMs for `dirvish-setup-menu'.
A ITEM is a list consists of (KEY VAR SCOPE DESCRIPTION) where
KEY is the keybinding for the item, VAR can be valid
attribute (as in `dirvish-attributes') or preview dispatcher (as
in `dirvish-preview-dispatchers') or a layout recipe (see
`dirvish-layout-recipes'), SCOPE can be `attr', `preview' or
`layout'.  DESCRIPTION is the documentation for the VAR."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k (remove nil v))
    (unless (require 'dirvish nil t) (setq v nil))
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attr)) v))
          (preview-alist (seq-filter (lambda (i) (eq (nth 2 i) 'preview)) v))
          (layout-alist (seq-filter (lambda (i) (eq (nth 2 i) 'layout)) v)))
      (cl-labels ((new-infix (i)
                    (let* ((infix-var (nth 1 i))
                           (infix-name (intern (format "dirvish-%s-infix" (nth 1 i))))
                           (slot-name (pcase (nth 2 i)
                                        ('attr "attributes")
                                        ('preview "preview-dispatchers")))
                           (infix-scope (intern (format "dv-%s" slot-name)))
                           (infix-desc (nth 3 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish-menu-toggles
                               :variable ',infix-var
                               :scope ',infix-scope
                               :description ,infix-desc))))
                  (expand-infix (i) (list (car i) (intern (format "dirvish-%s-infix" (nth 1 i)))))
                  (layout-option (i) (list (car i) (nth 3 i)
                                           `(lambda () (interactive) (dirvish-switch-layout ,(nth 1 i))))))
        (mapc #'new-infix attr-alist)
        (mapc #'new-infix preview-alist)
        (eval
         `(transient-define-prefix dirvish-setup-menu ()
            "Configure current Dirvish session."
            [:description
             (lambda () (dirvish-menu--format-heading "Setup Dirvish"))
             ["File attributes:"
              ,@(mapcar #'expand-infix attr-alist)]]
            ["Preview:" :if (lambda () (dv-layout (dirvish-curr)))
             ,@(mapcar #'expand-infix preview-alist)]
            [:if (lambda () (and (derived-mode-p 'dirvish-mode) (dv-layout (dirvish-curr))))
             :description
             (lambda ()
               (format "%s\n%s"
                       (propertize "Layout:" 'face 'transient-heading)
                       (propertize "Uppercased pane has the biggest size"
                                   'face 'font-lock-doc-face)))
             ,@(mapcar #'layout-option layout-alist)]
            ["Actions:"
             ("RET" "Confirm and quit"
              (lambda () (interactive) (dirvish-build (dirvish-curr)) (revert-buffer)))]
            (interactive)
            (if (or (derived-mode-p 'dirvish-mode)
                    (bound-and-true-p dirvish-override-dired-mode))
                (transient-setup 'dirvish-setup-menu)
              (user-error "`dirvish-setup-menu' is for Dirvish only"))))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here
