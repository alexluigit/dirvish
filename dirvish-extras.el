;;; dirvish-extras.el --- Extra utilities and transient prefixes for dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Extra utilities and transient prefixes for Dirvish.
;;
;; Commands included:
;; - `dirvish-find-file-true-path'
;; - `dirvish-copy-file-name' (autoload)
;; - `dirvish-copy-file-path' (autoload)
;; - `dirvish-copy-file-directory'
;; - `dirvish-total-file-size' (autoload)
;; - `dirvish-layout-toggle' (autoload)
;; - `dirvish-layout-switch' (autoload)
;; - `dirvish-rename-space-to-underscore'
;;
;; Transient prefixes included (all autoloaded):
;; - `dirvish-file-info-menu'
;; - `dirvish-renaming-menu'
;; - `dirvish-subdir-menu'
;; - `dirvish-chxxx-menu'
;; - `dirvish-mark-menu'
;; - `dirvish-epa-dired-menu'
;; - `dirvish-setup-menu'
;; - `dirvish-dired-cheatsheet'
;; - `dirvish-dispatch'

;;; Code:

(require 'dirvish)
(require 'transient)
(declare-function tramp-file-name-user "tramp")
(declare-function tramp-file-name-host "tramp")

(defcustom dirvish-layout-recipes
  '((0 0    0.4)   ;        | CURRENT | preview
    (0 0    0.8)   ;        | current | PREVIEW
    (1 0.08 0.8)   ; parent | current | PREVIEW
    (1 0.11 0.55)) ; parent | current | preview
  "Layout RECIPEs for `dirvish-layout-switch' command.
RECIPE has the same form as `dirvish-default-layout'."
  :group 'dirvish
  :type '(repeat (list (integer :tag "number of parent windows")
                       (float :tag "max width of parent windows")
                       (float :tag "width of preview window"))))

(defclass dirvish-attribute-set (transient-infix)
  ((variable :initarg :variable))
  "Class for dirvish attributes.")

(cl-defmethod transient-format-description ((obj dirvish-attribute-set))
  "Format description for DIRVISH-ATTRIBUTE instance OBJ."
  (format "%s%s" (oref obj description)
          (propertize " " 'display '(space :align-to (- right 5)))))

(cl-defmethod transient-format-value ((obj dirvish-attribute-set))
  "Format value for DIRVISH-ATTRIBUTE instance OBJ."
  (let* ((val (oref obj value))
         (face (if (equal val "+") 'transient-argument 'transient-inactive-value)))
    (propertize val 'face face)))

(cl-defmethod transient-init-value ((obj dirvish-attribute-set))
  "Initialize value for DIRVISH-ATTRIBUTE instance OBJ."
  (let ((sym (oref obj variable))
        (attrs (mapcar #'car (dirvish-prop :attrs))))
    (oset obj value (if (memq sym attrs) "+" "-"))))

(cl-defmethod transient-infix-read ((obj dirvish-attribute-set))
  "Read value from DIRVISH-ATTRIBUTE instance OBJ."
  (oset obj value (if (equal (oref obj value) "+") "-" "+")))

(cl-defmethod transient-infix-set ((obj dirvish-attribute-set) value)
  "Set relevant value in DIRVISH-ATTRIBUTE instance OBJ to VALUE."
  (mapc #'require '(dirvish-widgets dirvish-vc dirvish-collapse))
  (let* ((item (oref obj variable))
         (old-val (mapcar #'car (dirvish-prop :attrs)))
         (new-val (if (equal value "+") (cl-pushnew item old-val)
                    (remove item old-val))))
    (dirvish-prop :attrs (dirvish--attrs-expand new-val))))

;;;###autoload (autoload 'dirvish-setup-menu "dirvish-extras" nil t)
(defcustom dirvish-ui-setup-items
  '(("s"  file-size     "File size")
    ("t"  file-time     "File modification time")
    ("m"  file-modes    "File modes")
    ("c"  collapse      "Collapse unique nested paths"
     (not (dirvish-prop :remote)))
    ("v"  vc-state      "Version control state"
     (and (display-graphic-p) (symbolp (dirvish-prop :vc-backend))))
    ("l"  git-msg       "Git commit's short log"
     (and (symbolp (dirvish-prop :vc-backend)) (not (dirvish-prop :remote))))
    ("1" '(0 nil  0.4)  "     -       | current (60%) | preview (40%)")
    ("2" '(0 nil  0.8)  "     -       | current (20%) | preview (80%)")
    ("3" '(1 0.08 0.8)  "parent (8%)  | current (12%) | preview (80%)")
    ("4" '(1 0.11 0.55) "parent (11%) | current (33%) | preview (55%)"))
  "ITEMs for `dirvish-setup-menu'.
A ITEM is a list consists of (KEY VAR DESC PRED) where KEY is the
keybinding for the item, VAR can be a valid `dirvish-attributes'
or a layout recipe (see `dirvish-layout-recipes'), DESC is the
documentation for the VAR.  The optional PRED is passed as the
predicate for that infix."
  :group 'dirvish :type 'alist
  :set
  (lambda (key value)
    (set key value)
    (cl-loop
     with (attrs . layouts) = ()
     for (k v desc pred) in value
     for name = (and (symbolp v) (intern (format "dirvish-%s-infix" v)))
     do (if (not name)
            (push (list k (propertize desc 'face 'font-lock-doc-face)
                        `(lambda () (interactive) (dirvish-layout-switch ,v)))
                  layouts)
          (eval `(transient-define-infix ,name ()
                   :class 'dirvish-attribute-set :variable ',v
                   :description ,desc :if (lambda () ,(if pred `,@pred t))))
          (push (list k name) attrs))
     finally
     (eval
      `(transient-define-prefix dirvish-setup-menu ()
         "Configure current Dirvish session."
         [:description (lambda () (dirvish--format-menu-heading "Setup Dirvish UI"))
                       ["Attributes:" ,@attrs]]
         ["Switch layouts:"
          :if (lambda () (dv-curr-layout (dirvish-curr))) ,@layouts]
         ["Actions:"
          ("f" "Toggle fullscreen" dirvish-layout-toggle)
          ("a" "Apply current settings to future sessions"
           (lambda () (interactive)
             (let* ((dv (dirvish-curr)) (tp (dv-type dv)) (dft (eq tp 'default))
                    (attr-sym (or (and dft 'dirvish-attributes)
                                  (intern (format "dirvish-%s-attributes" tp))))
                    (attrs (mapcar #'car (dirvish-prop :attrs))))
               (when (boundp attr-sym) (set-default attr-sym attrs))
               (setq dirvish-default-layout (dv-ff-layout dv))
               (dirvish--build-layout (dirvish-curr))
               (revert-buffer))))]
         (interactive)
         (if (dirvish-curr) (transient-setup 'dirvish-setup-menu)
           (user-error "Not in a Dirvish buffer")))))))

(defun dirvish-find-file-true-path ()
  "Open truename of (maybe) symlink file under the cursor."
  (interactive)
  (dired-jump nil (file-truename (dired-get-filename nil t))))

(defun dirvish--kill-and-echo (string)
  "Echo last killed STRING."
  (kill-new string)
  (let ((hint (propertize
               "Copied: " 'face 'font-lock-builtin-face)))
    (message "%s" (format "%s%s" hint string))))

(defun dirvish-copy-file-true-path ()
  "Copy truename of (maybe) symlink file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (file-truename (dired-get-filename nil t))))

;;;###autoload
(defun dirvish-copy-file-name (&optional multi-line)
  "Copy filename of marked files.
If MULTI-LINE, make every name occupy a new line."
  (interactive "P")
  (let* ((files (dired-get-marked-files t))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

;;;###autoload
(defun dirvish-copy-file-path (&optional multi-line)
  "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a new line."
  (interactive "P")
  (let* ((files (mapcar #'file-local-name (dired-get-marked-files)))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

(defun dirvish-copy-remote-path (&optional multi-line)
  "Copy remote path of marked files.
If MULTI-LINE, every file takes a whole line."
  (interactive "P")
  (let* ((tramp (or (dirvish-prop :tramp)
                    (user-error "Not a remote folder")))
         (files (cl-loop for file in (dired-get-marked-files)
                         for user = (tramp-file-name-user tramp)
                         for host = (tramp-file-name-host tramp)
                         for localname = (file-local-name file)
                         collect (format "%s%s%s:%s" (or user "")
                                         (if user "@" "") host localname)))
         (names (mapconcat #'concat files (if multi-line "\n" " "))))
    (dirvish--kill-and-echo (if multi-line (concat "\n" names) names))))

(defun dirvish-copy-file-directory ()
  "Copy directory name of file under the cursor."
  (interactive)
  (dirvish--kill-and-echo
   (expand-file-name default-directory)))

;;;###autoload
(defun dirvish-total-file-size (&optional fileset)
  "Echo total file size of FILESET.
FILESET defaults to `dired-get-marked-files'."
  (interactive)
  (cl-labels ((f-name (f) (if (not (file-directory-p f)) f
                              (directory-files-recursively f ".*" nil t)))
              (f-size (f) (condition-case nil
                              (file-attribute-size (file-attributes f))
                            (file-error 0))))
    (let* ((fileset (or fileset (dired-get-marked-files)))
           (count (propertize (number-to-string (length fileset))
                              'face 'font-lock-builtin-face))
           (size (thread-last fileset (mapcar #'f-name) flatten-tree
                              (mapcar #'f-size) (cl-reduce #'+)
                              file-size-human-readable)))
      (message "%s" (format "Total size of %s entries: %s" count size)))))

;;;###autoload
(defun dirvish-layout-switch (&optional recipe)
  "Switch Dirvish layout according to RECIPE.
If RECIPE is not provided, switch to the recipe next to the
current layout defined in `dirvish-layout-recipes'."
  (interactive)
  (cl-loop
   with dv = (let ((dv (dirvish-curr)))
               (unless dv (user-error "Not in a Dirvish session"))
               (unless (dv-curr-layout dv)
                 (dirvish-layout-toggle)
                 (user-error "Dirvish: entering fullscreen")) dv)
   with old-recipe = (dv-curr-layout dv)
   with recipes = (if recipe (list recipe) dirvish-layout-recipes)
   with l-length = (length recipes)
   for idx from 1
   for recipe in recipes
   when (or (eq idx l-length) (equal old-recipe recipe))
   return
   (let* ((new-idx (if (> idx (1- l-length)) 0 idx))
          (new-recipe (nth new-idx recipes)))
     (setf (dv-curr-layout dv) new-recipe)
     (setf (dv-ff-layout dv) new-recipe)
     (dirvish--build-layout dv))))

(defun dirvish-rename-space-to-underscore ()
  "Rename marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (derived-mode-p 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (revert-buffer))
    (user-error "Not in a Dired buffer")))

(defun dirvish--marked-files-as-info-string ()
  "Return all marked files as a string."
  (let* ((files (dired-get-marked-files t))
         (count (length files)))
    (cond ((<= count 1)
           (format "current file: %s" (dired-get-filename t t)))
          ((<= count 10)
           (format "marked files:\n  %s" (mapconcat #'concat files "\n  ")))
          (t (format "marked files:\n  %s\n  ... and %s more (%s in total)"
                     (mapconcat #'concat (seq-take files 10) "\n  ")
                     (- count 10) count)))))

;;;###autoload (autoload 'dirvish-file-info-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-file-info-menu ()
  "Gather file information."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Get File Information"
          (dirvish--marked-files-as-info-string)))
   ("n"   "Copy file NAMEs in one line <n> / multiple lines <C-u n>"
    dirvish-copy-file-name)
   ("p"   "Copy file PATHs in one line <p> / multiple lines <C-u p>"
    dirvish-copy-file-path)
   ("P"   "Copy remote PATHs in one line <P> / multiple lines <C-u P>"
    dirvish-copy-remote-path
    :if (lambda () (dirvish-prop :remote)))
   ("d"   "Copy file DIRECTORY"                dirvish-copy-file-directory)
   ("l"   "Copy symlink's truename"            dirvish-copy-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("L"   "Go to symlink's truename"           dirvish-find-file-true-path
    :if (lambda () (file-symlink-p (dired-get-filename nil t))))
   ("s"   "Get total size of marked files"     dirvish-total-file-size)
   ("t"   "Show file TYPE"                     dired-show-file-type)])

(transient-define-prefix dirvish-subdir-menu ()
  "Help Menu for Dired subdir management."
  [:description
   (lambda () (dirvish--format-menu-heading "Manage subdirs"))
   ("i" "  Insert subdir"    dired-maybe-insert-subdir :transient t)
   ("k" "  Kill subdir"      dired-kill-subdir :transient t)
   ("n" "  Next subdir"      dired-next-subdir :transient t)
   ("p" "  Prev subdir"      dired-prev-subdir :transient t)
   ("j" "  Jump to subdir"   dired-goto-subdir)
   ("$" "  Hide subdir"      dired-hide-subdir :transient t)
   ("M-$" "Hide all subdirs" dired-hide-all)])

;;;###autoload (autoload 'dirvish-chxxx-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-chxxx-menu ()
  "Help Menu for file attribute modification commands."
  [:description
   (lambda () (dirvish--format-menu-heading "Modify file's attributes"))
   ("g"   "Change file's GROUP"          dired-do-chgrp)
   ("m"   "Change file's MODE"           dired-do-chmod)
   ("o"   "Change file's OWNER"          dired-do-chown)
   ("t"   "Change file's TIMESTAMP"      dired-do-touch)
   ("p"   "Change file's PATH"           dired-do-rename)])

;;;###autoload (autoload 'dirvish-mark-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-mark-menu ()
  "Help Menu for `dired-mark/do-*' commands."
  [["Mark or unmark files:"
    ("e" "  by Extension"                dired-mark-extension :transient t)
    ("*" "  by Regexp (file name)"       dired-mark-files-regexp :transient t)
    ("c" "  by Regexp (file content)"    dired-mark-files-containing-regexp :transient t)
    ("s" "  by Subdir"                   dired-mark-subdir-files :transient t)
    ("x" "  by Executable"               dired-mark-executables :transient t)
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
    ("O"   "Open"                        dired-do-find-marked-files)
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
    ("A"   "Modify file's attributes"    dirvish-chxxx-menu)
    ("C"   "Change mark type"            dired-change-marks)
    ("k"   "Kill lines"                  dired-do-kill-lines)]]
  (interactive)
  (require 'dired-x)
  (require 'dired-aux)
  (transient-setup 'dirvish-mark-menu))

;;;###autoload (autoload 'dirvish-renaming-menu "dirvish-extras" nil t)
(transient-define-prefix dirvish-renaming-menu ()
  "Help Menu for file renaming in Dired."
  [:description
   (lambda () (dirvish--format-menu-heading "File renaming"))
   ("u"   "Upper-case file name"          dired-upcase)
   ("l"   "Lower-case file name"          dired-downcase)
   ("_"   "Replace SPC with UNDERSCORE"   dirvish-rename-space-to-underscore :if-derived 'dirvish-mode)
   ("w"   "Enter wdired [writable dired]" wdired-change-to-wdired-mode :if-not-derived wdired-mode)])

(transient-define-prefix dirvish-epa-dired-menu ()
  "Help menu for `epa-dired-do-*' commands."
  [:description
   (lambda () (dirvish--format-menu-heading "GNUpg assistant"))
   ("e" "Encrypt" epa-dired-do-encrypt)
   ("d" "Decrypt" epa-dired-do-decrypt)
   ("v" "Verify"  epa-dired-do-verify)
   ("s" "Sign"    epa-dired-do-sign)])

;;;###autoload (autoload 'dirvish-dired-cheatsheet "dirvish-extras" nil t)
(transient-define-prefix dirvish-dired-cheatsheet ()
  "A collection of most frequently used Dired commands."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Dired cheatsheet"
          "The keys listed here may be different from the actual bindings"))
   ("n" "  Move to next line"      dired-next-line :transient t)
   ("p" "  Move to prev line"      dired-previous-line :transient t)
   (">" "  Move to next dirline"   dired-next-dirline :transient t)
   ("<" "  Move to prev dirline"   dired-prev-dirline :transient t)
   ("." "  Add an empty file"      dired-create-empty-file)
   ("+" "  Add a directory"        dired-create-directory)
   ("X" "  Delete files"           dired-do-delete)
   ("v" "  View this file"         dired-view-file)
   ("g" "  Refresh buffer"         revert-buffer)
   ("f" "  Find file"              dired-find-file)
   ("o" "  Find file other window" dired-find-file-other-window)
   ("j" "  Go to line for file"    dired-goto-file)
   ("^" "  Go to parent directory" dired-up-directory)
   ("=" "  Compare files"          dired-diff)
   ("(" "  Toggle details"         dired-hide-details-mode)
   ("d" "  Display this file"      dired-display-file)
   ("s" "  Manage subdirs"         dirvish-subdir-menu)
   (":" "  GnuPG helpers"          dirvish-epa-dired-menu)
   ("h" "  More info about Dired"  describe-mode)])

;;;###autoload (autoload 'dirvish-dispatch "dirvish-extras" nil t)
(transient-define-prefix dirvish-dispatch ()
  "Main menu for Dired/Dirvish."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Dirvish main menu"
          "NOTICE: these commands require relevant Dirvish extensions")
     (declare-function dirvish-narrow "dirvish-narrow"))
   "" "Actions & Essential commands"
   ("u" "User interface setup"   dirvish-setup-menu)
   ("c" "Dired cheatsheet"       dirvish-dired-cheatsheet)
   ("/" "Run fd search here"     dirvish-fd)
   ("#" "Search everything in ~" (lambda () (interactive)
                                   (dirvish-fd "~" "") (dirvish-narrow)))
   ("R" "Rsync marked files"     dirvish-rsync)
   ("n" "Live narrowing"         dirvish-narrow)
   "Transient commands"
   ("a" "Quick access"           dirvish-quick-access)
   ("h" "Go to history entries"  dirvish-history-menu)
   ("s" "Sort current buffer"    dirvish-quicksort)
   ("l" "Setup listing switches" dirvish-ls-switches-menu)
   ("f" "Setup fd-find switches" dirvish-fd-switches-menu
    :if (lambda () (dirvish-prop :fd-info)))
   ("S" "Setup rsync switches"   dirvish-rsync-switches-menu)
   ("m" "Manage marks"           dirvish-mark-menu)
   ("e" "Manage emerged groups"  dirvish-emerge-menu)
   ("t" "Manage subtrees"        dirvish-subtree-menu)
   ("r" "Rename files"           dirvish-renaming-menu)
   ("v" "Version control system" dirvish-vc-menu)
   ("y" "Yank marked files"      dirvish-yank-menu)
   ("i" "Get file information"   dirvish-file-info-menu)])

(provide 'dirvish-extras)
;;; dirvish-extras.el ends here
