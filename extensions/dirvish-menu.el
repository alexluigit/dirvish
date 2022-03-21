;;; dirvish-menu.el --- Keybindings menu for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (dirvish "0.9.9") (transient "3.6.7"))

;;; Commentary:

;;; This library is a Dirvish extension which provides transient-based help
;;; menu.  Most commands in the menu are just built-in Dired commands.

;;; Code:

(unless (require 'transient nil t)
  (user-error "Dirvish: help menu requires `transient.el'"))

(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'dirvish)
(defvar dirvish-menu-available-prefixs '(dirvish-setup-menu dirvish-goto-bookmark))

(defgroup dirvish-menu nil
  "Useful preset transient commands."
  :prefix "dirvish-menu-"
  :group 'dirvish)

(defclass dirvish--toggle (transient-infix)
  ((variable  :initarg :variable)
   (scope     :initarg :scope nil))
  "[Experimental] Class for Dirvish toggles.")

(cl-defmethod transient-format-description ((obj dirvish--toggle))
  (or (oref obj description) (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj dirvish--toggle))
  (propertize (prin1-to-string (oref obj value)) 'face 'transient-value))

(cl-defmethod transient-init-value ((obj dirvish--toggle))
  (oset obj value (if (memq (oref obj variable) (funcall (oref obj scope) (dirvish-curr))) 'On 'Off)))

(cl-defmethod transient-infix-read ((obj dirvish--toggle))
  (oset obj value (if (eq (oref obj value) 'On) 'Off 'On)))

(cl-defmethod transient-infix-set ((obj dirvish--toggle) value)
  (let* ((dv (dirvish-curr))
         (item (oref obj variable))
         (slot-name (oref obj scope))
         (curr-val (funcall slot-name dv))
         (new-val (if (eq value 'On) (push item curr-val) (delq item curr-val))))
    (cl-case slot-name
      ('dv-raw-attributes (setf (dv-raw-attributes dv) new-val))
      ('dv-raw-preview-dps (setf (dv-raw-preview-dps dv) new-val)))
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
    (dirvish-build)))

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
     ("s"   "Sort files"                          dired-sort-toggle-or-edit)
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
    ("t"   "show file TYPE"                       dired-show-file-type)
    ("l"   "find symlink's truename"              dirvish-find-file-true-path)])
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
  `(,(when dirvish--icon-backend `("i" ,dirvish--icon-backend attributes "File icons"))
    ("s" file-size      attributes   "File size (right-aligned)")
    ("g" vc-state       attributes   "VC state (left-aligned)")
    ("m" git-msg        attributes   "Git commit messages")
    ("e" expanded-state attributes   "Node expanded state")
    ("d" vc-diff        preview-dps  "VC diff")
    ("0" 0              column       "main column only")
    ("1" 1              column       "main + 1 parent (ranger like)")
    ("2" 2              column       "main + 2 parent")
    ("3" 3              column       "main + 3 parent")
    ("4" 4              column       "main + 4 parent"))
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
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attributes)) v))
          (preview-alist (seq-filter (lambda (i) (eq (nth 2 i) 'preview-dps)) v))
          (column-alist (seq-filter (lambda (i) (eq (nth 2 i) 'column)) v)))
      (cl-labels ((new-infix (i)
                    (let ((infix-var (nth 1 i))
                          (infix-name (intern (format "dirvish-%s-infix" (nth 1 i))))
                          (infix-scope (intern (format "dv-raw-%s" (nth 2 i))))
                          (infix-desc (nth 3 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish--toggle
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
             ("RET" "Confirm and quit" (lambda () (interactive) (dirvish-build) (revert-buffer)))]))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here
