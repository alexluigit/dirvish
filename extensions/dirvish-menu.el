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
  (oset obj value (if (memq (oref obj variable) (symbol-value (oref obj scope))) 'On 'Off)))

(cl-defmethod transient-infix-read ((obj dirvish--toggle))
  (oset obj value (if (eq (oref obj value) 'On) 'Off 'On)))

(cl-defmethod transient-infix-set ((obj dirvish--toggle) value)
  (if (eq value 'On)
      (push (oref obj variable) (symbol-value (oref obj scope)))
    (set (oref obj scope) (delq (oref obj variable) (symbol-value (oref obj scope)))))
  (dirvish--refresh-slots (dirvish-curr))
  (revert-buffer))

(defmacro dirvish-menu--transient-define-multi (spec)
  "Define transient command with core information from SPEC."
  `(prog1 'dirvish-menu
     ,@(mapcar
        (lambda (elm)
          (let ((pkg  (pop elm))
                (args elm))
            `(transient-define-prefix ,(intern (format "dirvish-menu-%s" pkg)) ()
               ,(format "Dirvish commands menu for `%s'." pkg)
               ,@args)))
        spec)))

(dirvish-menu--transient-define-multi
 ((all-cmds
   [:description
    (lambda ()
      (propertize (capitalize (format "%s help menu" (if (derived-mode-p 'dirvish-mode) "Dirvish" "Dired")))
                  'face '(:inherit dired-mark :height 1.2 :underline t)))
    :if-derived dired-mode
    ["File commands"
     ("e"   "Open file"                           dired-find-file)
     ("o"   "Open file other window"              dired-find-file-other-window)
     ("@"   "Rename files"                        dirvish-menu-renaming-cmds)
     ("w"   "Get file information"                dirvish-menu-file-info-cmds)
     ("f"   "Edit file attributes"                dirvish-menu-edit-file-attrs)
     ("s"   "Sort files"                          dired-sort-toggle-or-edit)]
    ["View"
     ("(" "  Hide detail info"                    dired-hide-details-mode)
     ("." "  Apply filters"                       dired-filter-mode :if (lambda () (featurep 'dired-filter)))
     ("." "  Apply filters"                       dired-omit-mode :if-not (lambda () (featurep 'dired-filter)))
     ("N" "  Live narrowing"                      dired-narrow :if (lambda () (featurep 'dired-narrow)))
     ("M-m" "Toggle fullscreen"                   dirvish-toggle-fullscreen :if-derived dirvish-mode)
     ("M-c" "Configure Dirvish UI"                dirvish-ui-config :if-derived dirvish-mode)
     ("M-l" "Change parent depth"                 dirvish-change-depth :if-derived dirvish-mode)]
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
     ("/" "  Goto common directories"             dirvish-goto)
     ("l" "  Goto last place"                     dirvish-other-buffer)
     ("SPC" "Recently visited"                    dirvish-show-history)]
    ["Mark"
     ("m" "  Mark current file"                   dired-mark)
     ("u" "  Unmark current file"                 dired-unmark)
     ("U" "  Unmark all"                          dired-unmark-all-marks)
     ("t" "  Toggle (invert) marks"               dired-toggle-marks)
     ("*" "  Mark by.."                           dirvish-menu-mark-by)
     ("SPC" "Actions on marks"                    dirvish-menu-action-on-marks)]
    ["Essential commands"
     ("C"   "Copy"                                dired-do-copy :if-mode dired-mode)
     ("C"   "Paste to current directory"          dirvish-yank :if-derived dirvish-mode)
     ("x"   "Delete"                              dired-do-delete)
     ("a"   "Add (create) an empty file"          dired-create-empty-file)
     ("v"   "View current file"                   dired-view-file)
     ("+"   "Create directory"                    dired-create-directory)
     ("g"   "Refresh buffer"                      revert-buffer)]
    ["Extensions"
     (":" "  GNUpg helpers"                       dirvish-menu-epa-dired-cmds)
     ("TAB" "Toggle subtree"                      dired-subtree-toggle :if (lambda () (fboundp 'dired-subtree-toggle)))
     ("=" "  Diffing with the other file"         dired-diff)]])
  (edit-file-attrs
   ["Change file attributes"
    ("R"   "Name"                                 dired-do-rename)
    ("G"   "Group"                                dired-do-chgrp)
    ("M"   "Mode"                                 dired-do-chmod)
    ("O"   "Owner"                                dired-do-chown)
    ("T"   "Timestamp"                            dired-do-touch)])
  (mark-by
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
  (action-on-marks
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
  (file-info-cmds
   ["Get file information"
    ("n"   "copy file NAME"                       dirvish-copy-file-name)
    ("p"   "copy file PATH"                       dirvish-copy-file-path)
    ("d"   "copy file DIRECTORY"                  dirvish-copy-file-directory)
    ("t"   "show file TYPE"                       dired-show-file-type)
    ("l"   "find symlink's truename"              dirvish-find-file-true-path)])
  (renaming-cmds
   ["File renaming"
    ("u"   "Upper-case file name"                 dired-upcase)
    ("l"   "Lower-case file name"                 dired-downcase)
    ("_"   "Replace SPC with UNDERSCORE"          dirvish-rename-space-to-underscore)
    ("w"   "Enter wdired [writable dired]"        wdired-change-to-wdired-mode :if-not-derived wdired-mode)])
  (epa-dired-cmds
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
           (window-parameters
            (no-other-window . t)))))
    (dirvish-menu-all-cmds)))

;;;###autoload (autoload 'dirvish-goto "dirvish-menu" nil t)
(defcustom dirvish-goto-dirs-alist
  '(("h" "~/"                          "Home")
    ("d" "~/Downloads/"                "Downloads")
    ("m" "/mnt/"                       "Drives")
    ("t" "~/.local/share/Trash/files/" "TrashCan"))
  "Predefined DIRs for `dirvish-goto'.
A DIR is a list consists of (KEY PATH DESCRIPTION) where KEY is a
string passed to `kbd', PATH is the the target for command
`dirvish-find-file', DESCRIPTION is a optional description for
the DIR.  See setter of this option for details."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    `(setq ,k v)
    (let* ((desc-len-seq (mapcar (lambda (i) (length (nth 2 i))) v))
           (max-desc-len (seq-max desc-len-seq)))
      (eval
       `(transient-define-prefix dirvish-goto ()
          "Open frequently visited directories using dirvish."
          ["Go to Directory: "
           ,@(cl-loop
              for (key path desc) in v
              collect
              (list key
                    (concat desc "  "
                            (make-string (- max-desc-len (length desc)) ?\ )
                            (propertize path 'face 'font-lock-comment-face))
                    `(lambda () (interactive) (dired-jump nil ,path))))])))))

;;;###autoload (autoload 'dirvish-ui-config "dirvish-menu" nil t)
(defcustom dirvish-ui-option-alist
  `(("i" ,dirvish-icon-backend attributes "File icons")
    ("m" git-msg               attributes "Git commit messages")
    ("g" vc-gutter             attributes "VC state at fringe")
    ("d" vc-diff               preview-dispatchers "VC diff at preview window"))
  "TOGGLEs for `dirvish-ui-config'.
A TOGGLE is a list consists of (KEY VAR SCOPE DESCRIPTION) where
KEY is a string passed to `kbd', VAR is a valid attribute (as in
`dirvish-attributes') or preview dispatcher (as in
`dirvish-preview-dispatchers'), SCOPE is either 'attributes or
'preview-dispatchers, DESCRIPTION is a optional description for
the VAR.  See setter of this option for details."
  :group 'dirvish :type 'alist
  :set
  (lambda (k v)
    `(setq ,k v)
    (let ((attr-alist (seq-filter (lambda (i) (eq (nth 2 i) 'attributes)) v))
          (preview-alist (seq-filter (lambda (i) (eq (nth 2 i) 'preview-dispatchers)) v)))
      (cl-labels ((new-infix (i)
                    (let ((infix-var (nth 1 i))
                          (infix-name (intern (format "dirvish-%s-infix" (nth 1 i))))
                          (infix-scope (intern (format "dirvish-%s" (nth 2 i))))
                          (infix-desc (nth 3 i)))
                      (eval `(transient-define-infix ,infix-name ()
                               :class 'dirvish--toggle
                               :variable ',infix-var
                               :scope ',infix-scope
                               :description ,infix-desc))))
                  (expand-infix (i) (list (car i) (intern (format "dirvish-%s-infix" (nth 1 i))))))
        (mapc #'new-infix attr-alist)
        (mapc #'new-infix preview-alist)
        (eval
         `(transient-define-prefix dirvish-ui-config ()
            "Change UI config of Dirvish."
            ["File attributes in body"
             ,@(mapcar #'expand-infix attr-alist)]
            ["Preview"
             ,@(mapcar #'expand-infix preview-alist)]
            ["Actions"
             ("RET" "Confirm and quit"
              (lambda () (interactive) (dirvish-build) (revert-buffer)))]))))))

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here
