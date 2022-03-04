;;; dirvish-menu.el --- Keybindings menu for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 0.9.9
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
(require 'dirvish-builder)

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
          (let ((pkg  (pop elm))
                (args elm))
            `(transient-define-prefix ,(intern (format "dirvish-%s-menu" pkg)) ()
               ,(format "Dirvish commands menu for `%s'." pkg)
               ,@args)))
        spec)))

;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-menu" nil t)
;;;###autoload (autoload 'dirvish-mark-actions-menu "dirvish-menu" nil t)
(dirvish-menu--transient-define-multi
 ((top-level
   [:description
    (lambda ()
      (propertize (capitalize (format "%s help menu" (if (derived-mode-p 'dirvish-mode) "Dirvish" "Dired")))
                  'face '(:inherit dired-mark :height 1.2 :underline t)))
    :if-derived dired-mode
    ["Essential commands"
     ("e"   "Open file"                           dired-find-file)
     ("o"   "Open file other window"              dired-find-file-other-window)
     ("w"   "Get file information"                dirvish-file-info-menu)
     ("s"   "Sort files"                          dired-sort-toggle-or-edit)
     ("v"   "View current file"                   dired-view-file)
     ("g"   "Refresh buffer"                      revert-buffer)]
    ["I/O commands"
     ("a"   "Add (create) an empty file"          dired-create-empty-file)
     ("C"   "Copy"                                dired-do-copy :if-mode dired-mode)
     ("C"   "Paste marked files here"             dirvish-yank :if-derived dirvish-mode)
     ("@"   "Rename files"                        dirvish-renaming-menu)
     ("X"   "Delete"                              dired-do-delete)
     ("f"   "Edit file attributes"                dirvish-file-attributes-menu)
     ("+"   "Create directory"                    dired-create-directory)]
    ["View"
     ("(" "  Hide detail info"                    dired-hide-details-mode)
     ("." "  Apply filters"                       dired-filter-mode :if (lambda () (featurep 'dired-filter)))
     ("." "  Apply filters"                       dired-omit-mode :if-not (lambda () (featurep 'dired-filter)))
     ("N" "  Live narrowing"                      dired-narrow :if (lambda () (featurep 'dired-narrow)))
     ("M-f" "Toggle fullscreen"                   dirvish-toggle-fullscreen :if-derived dirvish-mode)
     ("M-s" "Setup Dirvish"                       dirvish-setup-menu :if-derived dirvish-mode)
     ("M-c" "Collapse file names"                 dired-collapse-mode :if (lambda () (featurep 'dired-collapse)))]
    ["Subdirs"
     ("i" "    Insert subdir"                     dired-maybe-insert-subdir)
     ("K" "    Kill subdir"                       dired-kill-subdir)
     ("C-M-n" "Next subdir"                       dired-next-subdir)
     ("C-M-p" "Prev subdir"                       dired-prev-subdir)
     ("$" "    Hide subdir"                       dired-hide-subdir)
     ("M-$" "  Hide subdir all"                   dired-hide-all)]
    ["Navigation"
     ("j" "  Jump to file in buffer"              dired-goto-file)
     ("J" "  Jump to file in file system"         dirvish-browse-all-directories)
     ("^" "  Go up directory"                     dired-up-directory)
     ("r" "  'Roam' in file system"               dirvish-roam)
     ("l" "  Goto last place"                     dirvish-other-buffer)
     ("SPC" "Recently visited"                    dirvish-show-history)]
    ["Mark"
     ("m" "  Mark current file"                   dired-mark)
     ("u" "  Unmark current file"                 dired-unmark)
     ("U" "  Unmark all"                          dired-unmark-all-marks)
     ("t" "  Toggle (invert) marks"               dired-toggle-marks)
     ("*" "  Mark by.."                           dirvish-marking-menu)
     ("M-a" "Actions on marks"                    dirvish-mark-actions-menu)]
    ["Extensions"
     (":" "  GNUpg helpers"                       dirvish-epa-dired-menu)
     ("TAB" "Toggle subtree"                      dired-subtree-toggle :if (lambda () (fboundp 'dired-subtree-toggle)))
     ("=" "  Diffing files"                       dired-diff)]])
  (file-attributes
   ["Change file attributes"
    ("R"   "Name"                                 dired-do-rename)
    ("G"   "Group"                                dired-do-chgrp)
    ("M"   "Mode"                                 dired-do-chmod)
    ("O"   "Owner"                                dired-do-chown)
    ("T"   "Timestamp"                            dired-do-touch)])
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
    ("S"   "Symlink"                              dired-do-symlink)
    ("H"   "Hardlink"                             dired-do-hardlink)
    ("P"   "Print"                                dired-do-print)
    ("X"   "Delete flagged"                       dired-do-flagged-delete)
    ("F"   "Search file contents"                 dired-do-find-regexp)
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

;;;###autoload (autoload 'dirvish-roam "dirvish-menu" nil t)
(defcustom dirvish-roam-dirs-alist
  '(("h" "~/"                          "Home")
    ("d" "~/Downloads/"                "Downloads")
    ("m" "/mnt/"                       "Drives")
    ("t" "~/.local/share/Trash/files/" "TrashCan"))
  "Predefined DIRs for `dirvish-roam'.
A DIR is a list consists of (KEY PATH DESCRIPTION) where KEY is a
string passed to `kbd', PATH is the the target for command
`dirvish-find-file', DESCRIPTION is a optional description for
the DIR.  See setter of this option for details."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k v)
    (let ((max-desc-len (seq-max (mapcar (lambda (i) (length (nth 2 i))) v))))
      (eval
       `(transient-define-prefix dirvish-roam ()
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

(defconst dirvish--icon-backend (or (require 'vscode-icon nil t) (require 'all-the-icons nil t)))
;;;###autoload (autoload 'dirvish-setup-menu "dirvish-menu" nil t)
(defcustom dirvish-setup-menu-alist
  `(,(when dirvish--icon-backend `("i" ,dirvish--icon-backend attributes "File icons"))
    ("s" file-size      attributes   "File size")
    ("m" git-msg        attributes   "Git commit messages")
    ("g" vc-gutter      attributes   "VC state at fringe")
    ("d" vc-diff        preview-dps  "VC diff at preview window")
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
'preview-dispatchers, DESCRIPTION is a optional description for
the VAR.  See setter of this option for details."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    (set k (remove nil v))
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attributes)) v))
          (preview-alist (seq-filter (lambda (i) (eq (nth 2 i) 'preview-dispatchers)) v))
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
            ["File attributes:"
             ,@(mapcar #'expand-infix attr-alist)]
            ["Preview:"
             ,@(mapcar #'expand-infix preview-alist)]
            ["Layout:"
             :if (lambda () (and (derived-mode-p 'dirvish-mode) (not (dirvish-dired-p))))
             ,@(mapcar #'column-option column-alist)]
            ["Actions:"
             ("RET" "Confirm and quit" (lambda () (interactive) (dirvish-build) (revert-buffer)))]))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here
