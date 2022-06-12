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
(declare-function dv-roots "dirvish")
(require 'transient)
(require 'dirvish)

(defvar dirvish-fd-actual-switches nil)
(defvar dirvish-fd-last-input)
(defvar dirvish-fd-args-history)

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

;;;###autoload (autoload 'dirvish-dispatch "dirvish-menu" nil t)
(transient-define-prefix dirvish-dispatch ()
  "Main help menu for Dired/Dirvish."
  [:description
   (lambda () (dirvish-menu--format-heading
          (format "%s help menu" (if (derived-mode-p 'dirvish-mode) "Dirvish" "Dired")) 1.3))
   :if-derived dired-mode
   ["Essential commands"
    ("e" "  Open file"              dired-find-file)
    ("o" "  Open file other window" dired-find-file-other-window)
    ("/" "  Search for files"       dirvish-fd)
    ("s" "  Sort current buffer"    dirvish-quicksort)
    ("g" "  Refresh buffer"         revert-buffer)
    ("M-s" "Setup Dirvish"          dirvish-setup-menu)
    ("TAB" "Toggle subtree"         dirvish-toggle-subtree)
    ("M-f" "Toggle fullscreen"      dirvish-toggle-fullscreen :if-derived dirvish-mode)]
   ["File operations"
    ("a" "  Add an empty file"      dired-create-empty-file)
    ("+" "  Add a directory"        dired-create-directory)
    ("@" "  Rename files"           dirvish-renaming-menu)
    ("X" "  Delete files"           dired-do-delete)
    ("v" "  View this file"         dired-view-file)
    ("C" "  Copy"                   dired-do-copy :if-mode dired-mode)
    ("y" "  Yank marked files"      dirvish-yank-menu :if-derived dirvish-mode)
    ("." "  Filter by.."            dirvish-filter-menu :if (lambda () (featurep 'dired-filter)))
    ("." "  Toggle file omitting"   dired-omit-mode :if-not (lambda () (featurep 'dired-filter)))
    ("*" "  Manage marks"           dirvish-mark-menu)]]
  [["Navigation"
    ("j" "  Jump to line for file"  dired-goto-file)
    ("b" "  Go to bookmarks"        dirvish-bookmark-goto)
    ("^" "  Go to parent directory" dired-up-directory)
    ("r" "  Roam the file system"   dirvish-fd-roam :if (lambda () (featurep 'dirvish-fd)))
    ("m" "  Go to the MRU buffer"   dirvish-history-last :if (lambda () (featurep 'dirvish-history)))
    ("n" "  Forward history"        dirvish-history-go-forward
     :transient t :if (lambda () (featurep 'dirvish-history)))
    ("p" "  Backward history"       dirvish-history-go-backward
     :transient t :if (lambda () (featurep 'dirvish-history)))
    ("SPC" "Recently visited"       dirvish-history-jump :if (lambda () (featurep 'dirvish-history)))]
   ["Others"
    ("l" "  Setup listing switches" dirvish-ls-switches-menu)
    ("f" "  Setup fd switches"      dirvish-fd-switches-menu
     :if (lambda () (and (dirvish-prop :fd-dir) dirvish-fd-actual-switches)))
    ("i" "  Get file information"   dirvish-file-info-menu)
    ("S" "  Manage subdirs"         dirvish-subdir-menu)
    ("(" "  Toggle details"         dired-hide-details-mode)
    ("=" "  Compare files"          dired-diff)
    (":" "  GnuPG helpers"          dirvish-epa-dired-menu)
    ("N" "  Live narrowing"         consult-focus-lines :if (lambda () (featurep 'consult)))]])

;;;###autoload (autoload 'dirvish-ls-switches-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-ls-switches-menu ()
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
    ("B" "Scale sizes when printing, eg. 10K" "--block-size=")
    ""
    "toggles"
    ("r" "Reverse order while sorting" "--reverse")
    ("d" "List directories ontop" "--group-directories-first")
    ("~" "Hide backups files (eg. foo~)" "--ignore-backups")
    ("A" "Show the author" "--author")
    ("C" "Show security context" "--context")
    ("H" "Human readable file size" "--human-readable")
    ("G" "Hide group names" "--no-group")
    ("O" "Hide owner names" "-g")
    ("L" "Info for link references or link itself" "--dereference")
    ("N" "Numeric user and group IDs" "--numeric-uid-gid")
    ("P" "Powers of 1000 for file size rather than 1024" "--si")
    ("I" "Show index number" "--inode")
    ("S" "Show the allocated size" "--size")
    ""
    "Actions"
    ("RET" "  Apply to this buffer" dirvish-menu--apply-ls-switches-to-buffer)
    ("M-RET" "Apply to this session" dirvish-menu--apply-switches-to-session :if-derived 'dirvish-mode)
    ("C-r" "  Reset this buffer" dirvish-menu--reset-switches-for-buffer)
    ("M-r" "  Reset this session" dirvish-menu--reset-switches-for-session :if-derived 'dirvish-mode)
    ("C-l" "  Clear choices" dirvish-menu--clear-switches-choices :transient t)]])

(transient-define-prefix dirvish-fd-switches-menu ()
  "Setup fd switches."
  :init-value
  (lambda (o) (oset o value (split-string (or dirvish-fd-actual-switches ""))))
  [:description
   (lambda ()
     (let ((title "setup fd switches")
           (notes "Ignore Range (by default ignore ALL)
  VCS: .gitignore + .git/info/exclude + $HOME/.config/git/ignore
  ALL: VCS + .ignore + .fdignore + $HOME/.config/fd/ignore"))
       (format "%s\n%s" (dirvish-menu--format-heading title)
               (propertize notes 'face 'font-lock-doc-face))))
   ["File types (multiple types can be included)"
    (3 "f" " Search for regular files" "--type=file")
    (3 "d" " Search for directories" "--type=directory")
    (3 "l" " Search for symbolic links" "--type=symlink")
    (3 "s" " Search for sockets" "--type=socket")
    (3 "p" " Search for named pipes" "--type=pipe")
    (3 "x" " Search for executable" "--type=executable")
    (3 "e" " Search for empty files or directories" "--type=empty")
    ""
    "Toggles"
    (3 "-H" "Include hidden files|dirs in the results" "--hidden")
    (3 "-I" "Show results from ALL" "--no-ignore")
    (4 "iv" "Show results from VCS" "--no-ignore-vcs")
    (5 "ip" "Show results from .gitignore in parent dirs" "--no-ignore-parent")
    (3 "-s" "Perform a case-sensitive search" "--case-sensitive")
    (4 "-g" "Perform a glob-based (rather than regex-based) search" "--glob")
    (4 "-F" "Treat the pattern as a literal string" "--fixed-strings")
    (4 "-L" "Traverse symbolic links" "--follow")
    (4 "-p" "Let the pattern match against the full path" "--full-path")
    (5 "mr" "Maximum number of search results" "--max-results")
    (5 "mt" "Do not descend into a different file systems" "--mount")
    (5 "P" " Do not traverse into matching directories" "--prune")
    ""
    "Options"
    (4 "-e" dirvish-menu--fd-extensions)
    (4 "-E" dirvish-menu--fd-exclude)
    (4 "-D" "Max level for directory traversing" "--max-depth=")
    (5 "-d" "Only show results starting at the depth" "--mix-depth=")
    (5 "gd" "Only show results starting at the exact given depth" "--exact-depth=")
    (5 "if" "Add a custom ignore-file in '.gitignore' format" "--ignore-file="
       :reader (lambda (_prompt _init _hist) (read-file-name "Choose ignore file: ")))
    (5 "-S" "Limit results based on the size of files" "--size="
       :reader (lambda (_prompt _init _hist)
                 (read-string "Input file size using the format <+-><NUM><UNIT> (eg. +100m): ")))
    (5 "cn" "Filter results based on the file mtime newer than" "--changed-within="
       :reader (lambda (_prompt _init _hist)
                 (read-string "Input a duration (10h, 1d, 35min) or a time point (2018-10-27 10:00:00): ")))
    (5 "co" "Filter results based on the file mtime older than" "--changed-before="
       :reader (lambda (_prompt _init _hist)
                 (read-string "Input a duration (10h, 1d, 35min) or a time point (2018-10-27 10:00:00): ")))
    (6 "-o" "Filter files by their user and/or group" "--owner="
       :reader (lambda (_prompt _init _hist)
                 (read-string "user|uid:group|gid - eg. john, :students, !john:students ('!' means to exclude files instead): ")))
    ""
    "Actions"
    ("r" dirvish-menu--fd-search-pattern)
    ("RET" "Apply switches" dirvish-menu--apply-fd-switches)]])

(transient-define-prefix dirvish-subdir-menu ()
  "Help Menu for Dired subdir management."
  ["Manage subdirs"
   ("i" "  Insert subdir"    dired-maybe-insert-subdir :transient t)
   ("k" "  Kill subdir"      dired-kill-subdir :transient t)
   ("n" "  Next subdir"      dired-next-subdir :transient t)
   ("p" "  Prev subdir"      dired-prev-subdir :transient t)
   ("j" "  Jump to subdir"   dired-goto-subdir)
   ("$" "  Hide subdir"      dired-hide-subdir :transient t)
   ("M-$" "Hide all subdirs" dired-hide-all)])

;;;###autoload (autoload 'dirvish-mark-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-mark-menu ()
  "Help Menu for `dired-mark-*' commands."
  [["Mark or unmark files:"
    ("e" "  by Extension"                dired-mark-extension :transient t)
    ("%" "  by Regexp (file name)"       dired-mark-files-regexp :transient t)
    ("g" "  by Regexp (file content)"    dired-mark-files-containing-regexp :transient t)
    ("s" "  by Subdir"                   dired-mark-subdir-files :transient t)
    ("*" "  by Executable"               dired-mark-executables :transient t)
    ("/" "  by Directory"                dired-mark-directories :transient t)
    ("@" "  by Symlink"                  dired-mark-symlinks :transient t)
    ("&" "  by Garbage"                  dired-flag-garbage-files :transient t)
    ("#" "  by Auto-saved"               dired-flag-auto-save-files :transient t)
    ("~" "  by Backup"                   dired-flag-backup-files :transient t)
    ("." "  by Numerical backup"         dired-clean-directory :transient t)
    ("u" "  Unmark this file"            dired-unmark :transient t)
    ("DEL" "Unmark and move up line"     dired-unmark-backward :transient t)
    ("U" "  Unmark all files"            dired-unmark-all-files :transient t)
    ("t" "  Toggle marks"                dired-toggle-marks :transient t)
    ("n" "  Move to next marked file"    dired-next-marked-file :transient t)
    ("p" "  Move to prev marked file"    dired-prev-marked-file :transient t)]
   ["Actions on marked files:"
    ("F"   "Open"                        dired-do-find-marked-files)
    ("S"   "Symlink"                     dired-do-symlink)
    ("H"   "Hardlink"                    dired-do-hardlink)
    ("P"   "Print"                       dired-do-print)
    ("X"   "Delete flagged"              dired-do-flagged-delete)
    ("r"   "Search file contents"        dired-do-find-regexp)
    ("R"   "Replace file contents"       dired-do-find-regexp-and-replace)
    ("B"   "Byte compile elisp"          dired-do-byte-compile)
    ("L"   "Load elisp"                  dired-do-load)
    ("z"   "Compress to"                 dired-do-compress-to)
    ("Z"   "Compress"                    dired-do-compress)
    ("!"   "Shell command"               dired-do-shell-command)
    ("&"   "Async shell command"         dired-do-async-shell-command)
    ("N"   "Echo number of marked files" dired-number-of-marked-files)
    ("l"   "Redisplay all marked files"  dired-do-redisplay)
    ("c"   "Change mark type"            dired-change-marks)
    ("k"   "Kill lines"                  dired-do-kill-lines)]]
  ["Change file attributes:"
   ("R"   "Change file's NAME"           dired-do-rename)
   ("G"   "Change file's GROUP"          dired-do-chgrp)
   ("M"   "Change file's MODE"           dired-do-chmod)
   ("O"   "Change file's OWNER"          dired-do-chown)
   ("T"   "Change file's TIMESTAMP"      dired-do-touch)])

;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
(transient-define-prefix dirvish-file-info-menu ()
  "Gather file information."
  [:description
   (lambda ()
     (let ((title "get file information")
           (notes "C-u n: separate NAMEs into different lines
C-u p: separate PATHs into different lines "))
       (format "%s\n%s" (dirvish-menu--format-heading title)
               (propertize notes 'face 'font-lock-doc-face))))
   ("n"   "Copy file NAMEs"                dirvish-copy-file-name)
   ("p"   "Copy file PATHs"                dirvish-copy-file-path)
   ("d"   "Copy file DIRECTORY"            dirvish-copy-file-directory)
   ("l"   "Copy symlink's truename"        dirvish-copy-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("L"   "Go to symlink's truename"       dirvish-find-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("s"   "Get total size of marked files" dirvish-total-file-size)
   ("t"   "show file TYPE"                 dired-show-file-type)])

(transient-define-prefix dirvish-renaming-menu ()
  "Help Menu for file renaming in Dired."
  ["File renaming"
   ("u"   "Upper-case file name"          dired-upcase)
   ("l"   "Lower-case file name"          dired-downcase)
   ("_"   "Replace SPC with UNDERSCORE"   dirvish-rename-space-to-underscore :if-derived 'dirvish-mode)
   ("w"   "Enter wdired [writable dired]" wdired-change-to-wdired-mode :if-not-derived wdired-mode)])

(transient-define-prefix dirvish-filter-menu ()
  "Transient-based `dired-filter-map'."
  ["Filter by:"
   ("n" "  Name"         dired-filter-by-name)
   ("r" "  Regexp"       dired-filter-by-regexp)
   ("." "  Extension"    dired-filter-by-extension)
   ("h" "  Dotfiles"     dired-filter-by-dot-files)
   ("o" "  Omit"         dired-filter-by-omit)
   ("g" "  Garbage"      dired-filter-by-garbage)
   ("e" "  Predicate"    dired-filter-by-predicate)
   ("f" "  File"         dired-filter-by-file)
   ("d" "  Directory"    dired-filter-by-directory)
   ("m" "  Mode"         dired-filter-by-mode)
   ("s" "  Symlink"      dired-filter-by-symlink)
   ("x" "  Executable"   dired-filter-by-executable)
   ("i" "  Git ignored"  dired-filter-by-git-ignored)
   ""
   "Compose filters:"
   ("|" "  Or"           dired-filter-or)
   ("!" "  Negate"       dired-filter-negate)
   ("*" "  Decompose"    dired-filter-decompose)
   ("Tab" "Transpose"    dired-filter-transpose)
   ("p" "  Pop"          dired-filter-pop)
   ("/" "  Pop all"      dired-filter-pop-all)
   ("S" "  Save"         dired-filter-save-filters)
   ("D" "  Delete saved" dired-filter-delete-saved-filters)
   ("A" "  Add saved"    dired-filter-add-saved-filters)
   ("L" "  Load saved"   dired-filter-load-saved-filters)])

(transient-define-prefix dirvish-epa-dired-menu ()
  "Help menu for `epa-dired-do-*' commands."
  ["GNUpg assistant"
   ("e" "Encrypt" epa-dired-do-encrypt)
   ("d" "Decrypt" epa-dired-do-decrypt)
   ("v" "Verify"  epa-dired-do-verify)
   ("s" "Sign"    epa-dired-do-sign)])

;;;###autoload (autoload 'dirvish-filter-menu "dirvish-menu" nil t)

(defun dirvish-menu--clear-switches-choices ()
  "Reload the listing switches setup UI."
  (interactive)
  (transient-setup 'dirvish-ls-switches-menu))

(defun dirvish-menu--apply-ls-switches-to-buffer (&optional switches)
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
    (dolist (buf (mapcar #'cdr (dv-roots dv)))
      (with-current-buffer buf
        (setq dired-actual-switches switches)
        (revert-buffer)))
    (setf (dv-ls-switches dv) switches)))

(defun dirvish-menu--reset-switches-for-buffer ()
  "Reset listing switches for current buffer."
  (interactive)
  (dirvish-menu--apply-ls-switches-to-buffer dired-listing-switches))

(defun dirvish-menu--reset-switches-for-session ()
  "Reset listing switches for current buffer."
  (interactive)
  (dirvish-menu--apply-switches-to-session dired-listing-switches))

(defun dirvish-menu--apply-fd-switches ()
  "Apply fd SWITCHES to current buffer."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (switches (string-join args " ")))
    (setq dirvish-fd-actual-switches switches)
    (revert-buffer)))

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
  "Sort current buffer with Dired sort SWITCHES."
  (let* ((regexp "\\(--time=\\w+\\|--sort=\\w+\\|--reverse\\)\\( \\)?")
         (others (replace-regexp-in-string regexp "" dired-actual-switches))
         (new-switches (concat others " " switches)))
    (setq dired-actual-switches new-switches)
    (revert-buffer)))

(transient-define-infix dirvish-menu--fd-extensions ()
  :description "Filter results by file extensions"
  :class 'transient-option
  :argument "--extension="
  :multi-value 'repeat
  :prompt
  (lambda (o)
    (let* ((val (oref o value))
           (str (if val (format "(current: %s) " (mapconcat #'concat val ",")) "")))
      (format "%sFile exts separated with comma: " str))))

(transient-define-infix dirvish-menu--fd-exclude ()
  :description "Exclude files/dirs that match the glob pattern"
  :class 'transient-option
  :argument "--exclude="
  :multi-value 'repeat
  :prompt
  (lambda (o)
    (let* ((val (oref o value))
           (str (if val (format "(current: %s) " (mapconcat #'concat val ",")) "")))
      (format "%sGlob patterns (such as *.pyc) separated with comma: " str))))

(transient-define-infix dirvish-menu--fd-search-pattern ()
  "Change search pattern."
  :description "Change search pattern"
  :class 'transient-lisp-variable
  :variable 'dirvish-fd-last-input
  :reader (lambda (_prompt _init _hist)
            (completing-read "Input search pattern: "
                             dirvish-fd-args-history nil nil dirvish-fd-last-input)))

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

;;;###autoload (autoload 'dirvish-setup-menu "dirvish-menu" nil t)
(defcustom dirvish-menu-setup-items
  '(("s"  file-size      attr     "File size")
    ("c"  collapse       attr     "Collapse unique nested paths"
     (or (not (dirvish-prop :tramp)) (tramp-local-host-p (dirvish-prop :tramp))))
    ("v"  vc-state       attr     "Version control state information"
     (dirvish-prop :vc-backend))
    ("m"  git-msg        attr     "Git commit messages"
     (and (dirvish-prop :vc-backend) (not (dirvish-prop :tramp))))
    ("d"  vc-diff        preview  "Version control diff in preview window")
    ("1" '(0 nil  0.4)   layout   "       | CURRENT | preview")
    ("2" '(0 nil  0.8)   layout   "       | current | PREVIEW")
    ("3" '(1 0.08 0.8)   layout   "parent | current | PREVIEW")
    ("4" '(1 0.1  0.6)   layout   "parent | current | preview"))
  "ITEMs for `dirvish-setup-menu'.
A ITEM is a list consists of (KEY VAR SCOPE DESCRIPTION PRED)
where KEY is the keybinding for the item, VAR can be valid
attribute (as in `dirvish-attributes') or preview dispatcher (as
in `dirvish-preview-dispatchers') or a layout recipe (see
`dirvish-layout-recipes'), SCOPE can be `attr', `preview' or
`layout'.  DESCRIPTION is the documentation for the VAR.  PRED,
when present, is wrapped with a lambda and being put into the
`:if' keyword in that prefix or infix."
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
                           (infix-pred (nth 4 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish-menu-toggles
                               :variable ',infix-var
                               :scope ',infix-scope
                               :description ,(nth 3 i)
                               :if (lambda () ,(if infix-pred `,@infix-pred t))))))
                  (expand-infix (i) (list (car i) (intern (format "dirvish-%s-infix" (nth 1 i)))))
                  (layout-option (i) (list (car i) (nth 3 i)
                                           `(lambda () (interactive) (dirvish-switch-layout ,(nth 1 i)))
                                           :if `(lambda () ,(if (nth 4 i) `,@(nth 4 i) t)))))
        (mapc #'new-infix attr-alist)
        (mapc #'new-infix preview-alist)
        (eval
         `(transient-define-prefix dirvish-setup-menu ()
            "Configure current Dirvish session."
            [:description
             (lambda () (dirvish-menu--format-heading "Setup Dirvish"))
             ["Attributes:"
              ,@(mapcar #'expand-infix attr-alist)]]
            ["Preview:"
             :if (lambda () (and (not (dirvish-prop :tramp)) (dv-layout (dirvish-curr))))
             ,@(mapcar #'expand-infix preview-alist)]
            [:description
             (lambda ()
               (format "%s\n%s"
                       (propertize "Layout:" 'face 'transient-heading)
                       (propertize "Uppercased pane has the biggest size"
                                   'face 'font-lock-doc-face)))
             :if (lambda () (dv-layout (dirvish-curr)))
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
