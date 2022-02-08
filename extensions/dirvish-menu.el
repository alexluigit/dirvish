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

(defgroup dirvish-menu nil
  "Useful preset transient commands."
  :prefix "dirvish-menu-"
  :group 'dirvish)

(defmacro dirvish-menu--transient-define-multi (spec)
  "Define transient command with core information from SPEC."
  `(prog1 'dirvish-menu
     ,@(mapcar
        (lambda (elm)
          (let ((pkg  (pop elm))
                (args elm))
            `(transient-define-prefix ,(intern (format "dirvish-%s" pkg)) ()
               ,(format "Dirvish commands menu for `%s'." pkg)
               ,@args)))
        spec)))

(dirvish-menu--transient-define-multi
 ((dispatch-cmds
   [:description
    (lambda ()
      (propertize (capitalize (format "%s help menu\n" major-mode))
                  'face '(:inherit dired-mark :height 1.2)))
    :if-derived dired-mode
    ["Essential commands"
     ("e"   "Open file"                           dired-find-file)
     ("c"   "Copy"                                dired-do-copy :if-mode dired-mode)
     ("c"   "Paste to current directory"          dirvish-yank :if-derived dirvish-mode)
     ("x"   "Delete"                              dired-do-delete)
     ("E"   "Create empty file"                   dired-create-empty-file)
     ("+"   "Create directory"                    dired-create-directory)
     ("g"   "Refresh buffer"                      revert-buffer)]
    ["File commands"
     ("@"   "Edit file names"                     dirvish-renaming-cmds)
     ("w"   "Get file information"                dirvish-file-info-cmds)
     ("f"   "Edit file attributes"                dirvish-edit-file-attrs)
     ("o"   "Open file other window"              dired-find-file-other-window)
     ("v"   "View file in current window"         dired-view-file)
     ("="   "Diffing with other file"             dired-diff)]
    ["View"
     ("s" "  Sort files"                          dired-sort-toggle-or-edit)
     ("(" "  Hide detail info"                    dired-hide-details-mode)
     ("." "  Toggle filters"                      dired-filter-mode :if (lambda () (featurep 'dired-filter)))
     ("." "  Toggle filters"                      dired-omit-mode :if-not (lambda () (featurep 'dired-filter)))
     ("M-m" "Toggle fullscreen"                   dirvish-toggle-fullscreen :if-derived dirvish-mode)
     ("M-l" "Change parent depth"                 dirvish-change-depth)]
    ["Subdirs"
     ("i" "    Insert subdir"                     dired-maybe-insert-subdir)
     ("K" "    Kill subdir"                       dired-kill-subdir)
     ("C-M-n" "Next subdir"                       dired-next-subdir)
     ("C-M-p" "Prev subdir"                       dired-prev-subdir)
     ("$" "    Hide subdir"                       dired-hide-subdir)
     ("M-$" "  Hide subdir all"                   dired-hide-all)]
    ["Navigation"
     ("j"   "Jump to file in buffer"              dired-goto-file)
     ("J"   "Jump to file in file system"         dirvish-browse-all-directories)
     ("^"   "Go up directory"                     dired-up-directory)
     ("r"   "Goto common directories"             dirvish-goto)
     ("l"   "Goto last place"                     dirvish-other-buffer)
     ("SPC" "Recently visited"                    dirvish-show-history)]
    ["Mark"
     ("m" "  Mark current file"                   dired-mark)
     ("u" "  Unmark current file"                 dired-unmark)
     ("U" "  Unmark all"                          dired-unmark-all-marks)
     ("t" "  Toggle (invert) marks"               dired-toggle-marks)
     ("*" "  Mark by.."                           dirvish-mark-by)
     ("SPC" "Actions on marks"                    dirvish-action-on-marks)]
    ["Extensions"
     (":" "  GNUpg helpers"                       dirvish-epa-dired-cmds)
     ("TAB" "Toggle subtree"                      dired-subtree-toggle :if (lambda () (fboundp 'dired-subtree-toggle)))]])
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
    ("*"   "EXECUTABLES"                          dired-mark-executables)
    ("/"   "DIRECTORIES"                          dired-mark-directories)
    ("@"   "SYMLINKS"                             dired-mark-symlinks)
    ("e"   "EXTENSIONS"                           dired-mark-extension)
    ("&"   "GARBAGE"                              dired-flag-garbage-files)
    ("#"   "AUTO-SAVED"                           dired-flag-auto-save-files)
    ("~"   "BACKUP"                               dired-flag-backup-files)
    ("."   "NUMERICAL BACKUPS"                    dired-clean-directory)
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
    ("w"   "Enter wdired mode"                    wdired-change-to-wdired-mode)
    ("u"   "Upper-case file name"                 dired-upcase)
    ("l"   "Lower-case file name"                 dired-downcase)
    ("_"   "Replace SPC with UNDERSCORE"          dirvish-rename-space-to-underscore)])
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
    (dirvish-dispatch-cmds)))

;;;###autoload (autoload 'dirvish-goto "dirvish-menu" nil t)
(defcustom dirvish-goto-dirs-alist
  '(("h" "~/"                          "Home")
    ("d" "~/Downloads/"                "Downloads")
    ("m" "/mnt/"                       "Drives")
    ("t" "~/.local/share/Trash/files/" "TrashCan"))
  "Predefined DIRs for `dirvish-goto'.
A DIR is a list consists of (KEY PATH DESCRIPTION) where KEY has
the same format as in `define-key', PATH is the the target for
command `dirvish-find-file', DESCRIPTION is a optional
description for the DIR."
  :group 'dirvish :type 'list
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

(provide 'dirvish-menu)
;;; dirvish-menu.el ends here
