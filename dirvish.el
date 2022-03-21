;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2022 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.0.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; A minimalistic yet versatile file manager based on Dired.
;; This package gives Dired the following features:
;;
;; - Multiple window layouts
;; - Always available file preview
;; - Isolated sessions
;; - Asynchronous directory listing
;; - A modern and composable user interface

;;; Code:

(declare-function all-the-icons-dired-mode "all-the-icons-dired")
(defvar fd-dired-input-fd-args)
(defvar fd-dired-buffer-name-format)
(declare-function dired-filter--describe-filters "dired-filter")
(defvar dired-filter-show-filters)
(defvar dired-filter-revert)
(when (require 'dired-filter nil t)
  (setq dired-filter-show-filters nil)
  (setq dired-filter-revert 'always))
(require 'so-long)
(require 'mailcap)
(require 'image-mode)
(require 'face-remap)
(require 'ansi-color)
(require 'project)
(require 'ring)
(require 'dired-x)
(eval-when-compile
  (require 'subr-x)
  (require 'find-dired))
(mailcap-parse-mimetypes)

;;;; User Options

(defgroup dirvish nil "A better Dired." :group 'dired)

(defcustom dirvish-attributes '()
  "File attributes such as `file-size' showing in Dirvish file lines.
The attributes are defined by `dirvish-define-attribute', you can
get all available attributes by evaluating:

\(prog1 (mapc #'require `dirvish-extra-libs')
       (describe-variable 'dirvish--available-attrs))"
  :group 'dirvish :type '(repeat dirvish-attribute))

(defcustom dirvish-preview-dispatchers
  `(text gif image video audio epub archive ,(if (require 'pdf-tools nil t) 'pdf-tools 'pdf-preface))
  "List of preview dispatchers.
Preview dispatchers are defined by `dirvish-define-preview'.  It
holds a function that takes current filename and dirvish session
as arguments and gets called at runtime when the preview window
is available.  It controls how the preview content for certain
filetypes are generated, or it can decline to handle the file
name and leaving it for future dispatchers.  If none of the
dispatchers can handle the preview, the fallback dispatcher named
`default' is used.  For details see `dirvish-preview-dispatch'."
  :group 'dirvish :type 'hook)

(defcustom dirvish-preview-disabled-exts
  '("iso" "bin" "exe" "gpg" "elc" "eln")
  "Do not preview files end with these extensions."
  :group 'dirvish :type 'list)

(defcustom dirvish-cache-dir
  (concat (or (getenv "XDG_CACHE_HOME") (concat (getenv "HOME") "/.cache")) "/dirvish/")
  "Preview / thumbnail cache directory for dirvish."
  :group 'dirvish :type 'string)

(defcustom dirvish-async-listing-threshold most-positive-fixnum
  "Threshold to invoke async directory listing.
For example, if the value is 10000, then directories with over
10000 files will be opened asynchronously."
  :group 'dirvish :type 'integer)

(defvar dirvish--history-ring nil)
(defcustom dirvish-history-length 30
  "Length of directory visiting history Dirvish will track.
Set it to nil disables the history tracking."
  :group 'dirvish :type '(choice (integer nil))
  :set (lambda (k v) (set k v) (setq dirvish--history-ring (and v (make-ring v)))))

(defcustom dirvish-depth 1
  "Level of directories to traverse up."
  :group 'dirvish :type 'integer)

(defcustom dirvish-parent-max-width 0.12
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-preview-width 0.6
  "Fraction of frame width taken by preview window."
  :group 'dirvish :type 'float)

(defcustom dirvish-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR).
Where DISK is path to a disk and TRASH-DIR is its corresponding
trash directory.
For example, if you set it to:

'((\"/mnt/HDD/\" . \".Trash/files\")
  (\"/mnt/CloudDrive\" . \".recycle\"))

Dirvish take /mnt/HDD/.Trash/files as your trash can when you are
in /mnt/HDD directory or its child entries. This can speed up
file deletion when you have multiple disk drives."
  :group 'dirvish :type 'alist)

(defcustom dirvish-header-string-function 'dirvish-default-header-string-fn
  "Function that returns content (a string) in Dirvish header."
  :group 'dirvish :type 'function)

(define-obsolete-variable-alias 'dirvish-use-large-header 'dirvish-header-style "0.8")

(defcustom dirvish-header-style 'large
  "Display STYLE used for header in a full-frame dirvish instance.
STYLE should be one of these:
- nil, which means do not show the header.
- `normal', header has the same fontsize as body.
- `large', scale fontsize in header with 125%."
  :group 'dirvish :type 'symbol
  :options '(nil large normal))

(defface dirvish-hl-line
  '((((class color) (background light)) :background "#8eecf4" :extend t)
    (((class color) (background dark)) :background "#004065" :extend t))
  "Face for Dirvish line highlighting."
  :group 'dirvish)

(defcustom dirvish-face-remap-alist
  '((header-line :height 1.04 :box (:line-width 4 :color "#303030")))
  "Face remapping alist used in dirvish window.
See `face-remapping-alist' for more details."
  :group 'dirvish :type 'alist)

(define-obsolete-variable-alias 'dirvish-footer-format 'dirvish-mode-line-format "0.9.9")

(defcustom dirvish-mode-line-format
  '(((:eval (dirvish--mode-line-sorter))
     (:eval (dirvish--mode-line-filter)))
    .
    ((:eval (dirvish--mode-line-index))))
  "Template for displaying mode line in Dirvish instance.
The value is a (LEFT . RIGHT) cons where LEFT/RIGHT has the same
format as `mode-line-format'.  Set it to nil hides the footer."
  :group 'dirvish :type '(choice (nil cons)))

(defvar dirvish-preview-setup-hook nil
  "Hook functions for preview buffer initialization.")

(defvar dirvish-activation-hook nil
  "Hook runs after activation of a Dirvish session.")

(defvar dirvish-deactivation-hook nil
  "Hook runs after deactivation of a Dirvish session.")

;;;; Internal variables

(defvar dirvish-advice-alist
  '((files         find-file                       dirvish-find-file-ad           :before)
    (dired         dired                           dirvish-dired-ad)
    (dired         dired-jump                      dirvish-dired-jump-ad)
    (dired         dired-find-file                 dirvish-find-file              :override)
    (dired         dired-find-alternate-file       dirvish-find-file              :override)
    (dired         dired-other-window              dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab                 dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame               dirvish-dired-other-frame-ad   :override)
    (dired         dired-up-directory              dirvish-up-directory           :override)
    (dired         dired-sort-toggle-or-edit       dirvish-sort-by-criteria       :override)
    (dired         +dired/quit-all                 quit-window                    :override)
    (dired         dired-view-file                 dirvish--enlarge               :before)
    (dired         dired-internal-do-deletions     dirvish-deletion-ad)
    (dired-aux     dired-dwim-target-next          dirvish-dwim-target-next-ad    :override)
    (wdired        wdired-change-to-wdired-mode    dirvish-wdired-mode-ad         :after)
    (wdired        wdired-exit                     dirvish-setup                  :after)
    (wdired        wdired-finish-edit              dirvish-setup                  :after)
    (wdired        wdired-abort-changes            dirvish-setup                  :after)
    (find-dired    find-dired-sentinel             dirvish-find-dired-sentinel-ad :after)
    (recentf       recentf-track-opened-file       dirvish-ignore-ad)
    (recentf       recentf-track-closed-file       dirvish-ignore-ad)
    (winner        winner-save-old-configurations  dirvish-ignore-ad)
    (dired-subtree dired-subtree-remove            dirvish-subtree-remove-ad)
    (evil          evil-refresh-cursor             dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor             dirvish-refresh-cursor-ad)
    (fd-dired      fd-dired                        dirvish-fd-dired-ad)
    (magit         magit-status-setup-buffer       dirvish--enlarge               :before)
    (lsp-mode      lsp-deferred                    dirvish-ignore-ad)))
(defvar dirvish-scopes '(:tab tab-bar--current-tab-index :frame selected-frame))
(defvar dirvish-hook-alist
  '((emacs window-selection-change-functions dirvish-reclaim)
    (emacs tab-bar-tab-pre-close-functions   dirvish--deactivate-for-tab)
    (emacs delete-frame-functions            dirvish--deactivate-for-frame)))
(defvar dirvish-debug-p nil)
(defvar dirvish-override-dired-mode nil)
(defvar dirvish-extra-libs '(dirvish-extras dirvish-vc))
(defconst dirvish--prefix-spaces 2)
(defconst dirvish--debouncing-delay 0.02)
(defconst dirvish--preview-img-threshold (* 1024 1024 0.5))
(defconst dirvish--preview-img-scale 0.92)
(defconst dirvish--repeat-interval 0.1)
(defconst dirvish--saved-new-tab-choice tab-bar-new-tab-choice)
(defconst dirvish--builtin-attrs '(hl-line symlink-target))
(defconst dirvish--header-remap-alist '((mode-line-inactive :inherit (mode-line) :height 1.99)))
(defconst dirvish--footer-remap-alist '((mode-line-inactive mode-line)))
(defconst dirvish--os-windows-p (memq system-type '(windows-nt ms-dos)))
(defconst dirvish--subtree-prefix-len
  (condition-case nil (length (or (bound-and-true-p dired-subtree-line-prefix) "  ")) (error 2)))
(defvar dirvish--transient-dvs '())
(defvar dirvish--repeat-timers '())
(defvar dirvish--available-attrs '())
(defvar-local dirvish--dired-async-p nil)
(defvar-local dirvish--child-entry nil)
(defvar-local dirvish--curr-name nil)
(defvar-local dirvish--vc-backend nil)
(defvar-local dirvish--attrs-width `(,dirvish--prefix-spaces . 0))
(defvar-local dirvish--attrs-alist nil)
(put 'dired-subdir-alist 'permanent-local t)
(put 'dirvish--child-entry 'permanent-local t)
(put 'dirvish--curr-name 'permanent-local t)

;;;; Helpers

(defmacro dirvish-repeat (func delay interval &rest args)
  "Execute FUNC with ARGS in every INTERVAL after DELAY."
  (let ((timer (intern (format "%s-timer" func))))
    `(progn
       (defvar ,timer nil)
       (add-to-list 'dirvish--repeat-timers ',timer)
       (setq ,timer (run-with-timer ,delay ,interval ',func ,@args)))))

(defmacro dirvish-debounce (label &rest body)
  "Debouncing the execution of BODY.
The BODY runs after the idle time `dirvish--debouncing-delay'.
Multiple calls under the same LABEL are ignored."
  (declare (indent defun))
  (let* ((timer (intern (format "dirvish-%s-debouncing-timer" label)))
         (do-once `(lambda () (unwind-protect ,@body (setq ,timer nil)))))
    `(progn
       (defvar ,timer nil)
       (unless (timerp ,timer)
         (setq ,timer (run-with-idle-timer dirvish--debouncing-delay nil ,do-once))))))

(defun dirvish-apply-ansicolor-h (_win pos)
  "Update dirvish ansicolor in preview window from POS."
  (with-current-buffer (current-buffer)
    (ansi-color-apply-on-region
     pos (progn (goto-char pos) (forward-line (frame-height)) (point)))))

(defun dirvish--ensure-path (&optional path)
  "Return a valid file path based on PATH."
  (let ((f (or path buffer-file-name)))
    (expand-file-name (if f (file-name-directory f) default-directory))))

(defmacro dirvish--hide-dired-header (&rest body)
  "Execute BODY then hide the Dired header."
  `(progn
     (remove-overlays (point-min) (point-max) 'dirvish-remove-header t)
     ,@body
     (save-excursion
       (goto-char (point-min))
       (let ((o (make-overlay (point) (progn (forward-line 1) (point)))))
         (overlay-put o 'dirvish-remove-header t)
         (overlay-put o 'invisible t)))))

(defun dirvish--shell-to-string (program &rest args)
  "Execute PROGRAM with arguments ARGS and return output string.
If program returns non zero exit code return nil."
  (let* ((exit-code nil)
         (output
          (with-output-to-string
            (with-current-buffer standard-output
              (setq exit-code (apply #'process-file program nil t nil args))))))
    (when (eq exit-code 0) output)))

(defun dirvish--display-buffer (buffer alist)
  "Try displaying BUFFER with ALIST.
This splits the window at the designated side of the frame.
ALIST is window arguments passed to `window--display-buffer'."
  (let* ((side (cdr (assq 'side alist)))
         (window-configuration-change-hook nil)
         (width (or (cdr (assq 'window-width alist)) 0.5))
         (height (cdr (assq 'window-height alist)))
         (size (or height (ceiling (* (frame-width) width))))
         (split-width-threshold 0)
         (mode-line-format nil)
         (new-window (split-window-no-error nil size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun dirvish--ensure-temp-buffer ()
  "Ensure a temporary buffer."
  (get-buffer-create " *Dirvish-temp*"))

(defun dirvish--get-project-root ()
  "Get root path of current project."
  (if (< emacs-major-version 29)
      (cdr-safe (project-current))
    (when-let ((pj (project-current))) (project-root pj))))

(defun dirvish--get-parent (path)
  "Get parent directory of PATH."
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun dirvish--actual-string-length (string)
  "Get STRING's actual display length."
  (/ (+ (length string) (string-bytes string)) 2))

(defun dirvish--get-subtree-depth ()
  "Get subtree depth at point."
  (apply #'max (append (cl-loop for ov in (overlays-at (point))
                                collect (or (overlay-get ov 'dired-subtree-depth) 0)) '(0))))

(defun dirvish--subtree-expanded-p ()
  "70x Faster version of `dired-subtree--is-expanded-p'."
  (save-excursion (< (dirvish--get-subtree-depth)
                     (progn (forward-line 1) (dirvish--get-subtree-depth)))))

(defun dirvish--get-filesize (fileset)
  "Determine file size of provided list of files in FILESET."
  (unless (executable-find "du") (user-error "`du' executable not found"))
  (with-temp-buffer
    (apply #'call-process "du" nil t nil "-sch" fileset)
    (format "%s" (progn (re-search-backward "\\(^[0-9.,]+[a-zA-Z]*\\).*total$")
                        (match-string 1)))))

(defun dirvish--get-trash-dir ()
  "Get trash directory for current disk."
  (cl-dolist (dir dirvish-trash-dir-alist)
    (when (string-prefix-p (car dir) (dired-current-directory))
      (cl-return (concat (car dir) (cdr dir))))))

(defun dirvish--append-metadata (metadata completions)
  "Append METADATA for minibuffer COMPLETIONS."
  (let ((entry (if (functionp metadata)
                   `(metadata (annotation-function . ,metadata))
                 `(metadata (category . ,metadata)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

;;;; Core

(defun dirvish-curr (&optional frame)
  "Get current dirvish instance in FRAME.
FRAME defaults to current frame."
  (if dirvish--curr-name
      (gethash dirvish--curr-name (dirvish-hash))
    (frame-parameter frame 'dirvish--curr)))

(defun dirvish-drop (&optional frame)
  "Drop current dirvish instance in FRAME.
FRAME defaults to current frame."
  (set-frame-parameter frame 'dirvish--curr nil))

(defmacro dirvish--get-util-buffer (dv type &rest body)
  "Return dirvish session DV's utility buffer with TYPE.
If BODY is non-nil, create the buffer and execute BODY in it."
  (declare (indent defun))
  `(progn
     (let* ((id (dv-name ,dv))
            (h-name (format " *Dirvish-%s-%s*" ,type id))
            (buf (get-buffer-create h-name)))
       (with-current-buffer buf ,@body buf))))

(defun dirvish--init-util-buffers (dv)
  "Initialize util buffers for DV."
  (dirvish--get-util-buffer dv 'preview
    (setq-local cursor-type nil)
    (setq-local mode-line-format nil)
    (add-hook 'window-scroll-functions #'dirvish-apply-ansicolor-h nil :local))
  (dirvish--get-util-buffer dv 'header
    (setq-local cursor-type nil)
    (setq-local header-line-format nil)
    (setq-local window-size-fixed 'height)
    (setq-local face-font-rescale-alist nil)
    (setq-local mode-line-format '((:eval (dirvish--apply-header-style))))
    (set (make-local-variable 'face-remapping-alist) dirvish--header-remap-alist))
  (dirvish--get-util-buffer dv 'footer
    (setq-local cursor-type nil)
    (setq-local header-line-format nil)
    (setq-local window-size-fixed 'height)
    (setq-local face-font-rescale-alist nil)
    (setq-local mode-line-format '((:eval (dirvish--format-mode-line))))
    (set (make-local-variable 'face-remapping-alist) dirvish--footer-remap-alist)))

(defun dirvish-reclaim (&optional _window)
  "Reclaim current dirvish."
  (unless (active-minibuffer-window)
    (if dirvish--curr-name
        (progn
          (dirvish--init-util-buffers (dirvish-curr))
          (or dirvish-override-dired-mode (dirvish--add-advices)))
      (or dirvish-override-dired-mode (dirvish--remove-advices)))
    (let ((dv (gethash dirvish--curr-name (dirvish-hash))))
      (set-frame-parameter nil 'dirvish--curr dv) dv)))

;;;###autoload
(cl-defmacro dirvish-define-attribute (name &key if form left right doc)
  "Define a Dirvish attribute NAME.
An attribute contains a pair of predicate/rendering functions
that are being called on `post-command-hook'.  The predicate fn
takes current session DV as argument and execute IF once.  When
IF evaluates to t, the rendering fn runs FORM for every line with
following arguments:

- `f-name'  from `dired-get-filename'
- `f-attrs' from `file-attributes'
- `f-beg'   from `dired-move-to-filename'
- `f-end'   from `dired-move-to-end-of-filename'
- `l-beg'   from `line-beginning-position'
- `l-end'   from `line-end-position'
- `hl-face' a face that is only passed in on current line
Optional keywords LEFT, RIGHT and DOC are supported."
  (declare (indent defun))
  (let* ((ov (intern (format "dirvish-%s-ov" name)))
         (pred (intern (format "dirvish-attribute-%s-pred" name)))
         (render (intern (format "dirvish-attribute-%s-rd" name)))
         (args '(f-name f-attrs f-beg f-end l-beg l-end hl-face))
         (pred-body (if (> (length if) 0) if t)))
    `(progn
       (add-to-list
        'dirvish--available-attrs
        (cons ',name '(:doc ,doc :left ,left :right ,right :overlay ,ov :if ,pred :fn ,render)))
       (defun ,pred (dv) (ignore dv) ,pred-body)
       (defun ,render ,args (ignore ,@args) (let ((ov ,form)) (and ov (overlay-put ov ',ov t)))))))

(defmacro dirvish-get-attribute-create (file attribute force &rest body)
  "Get FILE's ATTRIBUTE from `dirvish--attrs-alist'.
When FORCE or the attribute does not exist, set it with BODY."
  (declare (indent defun))
  `(let ((f-name ,file)
         (item (alist-get f-name dirvish--attrs-alist nil nil #'string=)))
     (unless item (push (list f-name :init t) dirvish--attrs-alist))
     (when (or ,force (not (plist-get item ,attribute)))
       (plist-put (alist-get f-name dirvish--attrs-alist nil nil #'string=) ,attribute ,@body))
     (plist-get (alist-get f-name dirvish--attrs-alist nil nil #'string=) ,attribute)))

(cl-defmacro dirvish-define-preview (name arglist &optional docstring &rest body)
  "Define a Dirvish preview dispatcher NAME.
A dirvish preview dispatcher is a function consumed by
`dirvish-preview-dispatch' which optionally takes
`file' (filename under the cursor) and `dv' (current Dirvish
session) as argument specified in ARGLIST.  DOCSTRING and BODY is
the docstring and body for this function."
  (declare (indent defun) (doc-string 3))
  (let* ((dp-name (intern (format "dirvish-%s-preview-dp" name)))
         (default-arglist '(file dv))
         (ignore-list (cl-set-difference default-arglist arglist)))
    `(progn (defun ,dp-name ,default-arglist ,docstring (ignore ,@ignore-list) ,@body))))

(defun dirvish-hash (&optional frame)
  "Return a hash containing all dirvish instance in FRAME.
The keys are the dirvish's names automatically generated by
`cl-gensym'.  The values are dirvish structs created by
`make-dirvish'.

FRAME defaults to the currently selected frame."
  ;; XXX: This must return a non-nil value to avoid breaking frames initialized
  ;; with after-make-frame-functions bound to nil.
  (or (frame-parameter frame 'dirvish--hash)
      (make-hash-table)))

(defun dirvish-get-all (slot &optional all-frame flatten)
  "Gather slot value SLOT of all Dirvish in `dirvish-hash'.
If ALL-FRAME is non-nil, collect for all frames.
If FLATTEN is non-nil, collect them as a flattened list."
  (let* ((dv-slot (intern (format "dv-%s" slot)))
         (all-vals (if all-frame
                       (mapcar (lambda (fr)
                                 (with-selected-frame fr
                                   (mapcar dv-slot (hash-table-values (dirvish-hash)))))
                               (frame-list))
                     (mapcar dv-slot (hash-table-values (dirvish-hash))))))
    (if flatten (delete-dups (flatten-tree all-vals)) (apply #'append all-vals))))

(cl-defstruct
    (dirvish
     (:conc-name dv-)
     (:constructor
      make-dirvish
      (&key
       (path nil)
       (depth dirvish-depth)
       (transient nil)
       (type nil)
       (dedicated nil)
       &aux
       (fullscreen-depth (if (>= depth 0) depth dirvish-depth))
       (read-only-depth (if (>= depth 0) depth dirvish-depth))
       (root-window-fn (let ((fn (intern (format "dirvish-%s-root-window-fn" type))))
                         (if (functionp fn) fn #'frame-selected-window)))
       (header-string-fn (let ((fn (intern (format "dirvish-%s-header-string-fn" type))))
                           (if (functionp fn) fn (symbol-value 'dirvish-header-string-function))))
       (find-file-window-fn (let ((fn (intern (format "dirvish-%s-find-file-window-fn" type))))
                              (if (functionp fn) fn #'selected-window)))
       (quit-window-fn (let ((fn (intern (format "dirvish-%s-quit-window-fn" type))))
                         (if (functionp fn) fn #'ignore))))))
  "Define dirvish data type."
  (name
   (cl-gensym)
   :documentation "is a symbol that is unique for every instance.")
  (depth
   dirvish-depth
   :documentation "TODO.")
  (fullscreen-depth
   dirvish-depth
   :documentation "TODO.")
  (read-only-depth
   dirvish-depth
   :read-only t :documentation "TODO.")
  (scopes
   ()
   :documentation "TODO.")
  (transient
   nil
   :documentation "TODO.")
  (type
   nil
   :documentation "TODO")
  (dedicated
   nil
   :documentation "TODO")
  (dired-buffers
   ()
   :documentation "holds all dired buffers in this instance.")
  (dired-windows
   ()
   :documentation "holds all dired windows in this instance.")
  (preview-window
   nil
   :documentation "is the window to display preview buffer.")
  (preview-buffers
   ()
   :documentation "holds all file preview buffers in this instance.")
  (window-conf
   (current-window-configuration)
   :documentation "is the window configuration given by `current-window-configuration'.")
  (root-window-fn
   #'frame-selected-window
   :documentation "is the main dirvish window.")
  (header-string-fn
   (symbol-value 'dirvish-header-string-function)
   :documentation "TODO.")
  (find-file-window-fn
   #'selected-window
   :documentation "TODO.")
  (quit-window-fn
   #'ignore
   :documentation "TODO.")
  (root-window
   nil
   :documentation "is the main dirvish window.")
  (root-dir-buf-alist
   ()
   :documentation "TODO.")
  (parent-dir-buf-alist
   ()
   :documentation "TODO.")
  (raw-attributes
   (purecopy dirvish-attributes)
   :documentation "TODO.")
  (attributes-alist
   ()
   :documentation "TODO.")
  (path
   nil
   :documentation "TODO.")
  (index-path
   ""
   :documentation "is the file path under cursor in ROOT-WINDOW.")
  (raw-preview-dps
   (purecopy dirvish-preview-dispatchers)
   :documentation "TODO.")
  (preview-dispatchers
   ()
   :documentation "Preview dispatchers used for preview in this instance.")
  (ls-switches
   dired-listing-switches
   :documentation "is the list switches passed to `ls' command.")
  (sort-criteria
   (cons "default" "")
   :documentation "is the addtional sorting flag added to `dired-list-switches'."))

(defmacro dirvish-new (&rest args)
  "Create a new dirvish struct and put it into `dirvish-hash'.
ARGS is a list of keyword arguments followed by an optional BODY.
The keyword arguments set the fields of the dirvish struct.
If BODY is given, it is executed to set the window configuration
for the dirvish.

Save point, and current buffer before executing BODY, and then
restore them after."
  (declare (indent defun))
  (let ((keywords))
    (while (keywordp (car args))
      (dotimes (_ 2) (push (pop args) keywords)))
    (setq keywords (reverse keywords))
    `(let ((dv (make-dirvish ,@keywords)))
       (unless (frame-parameter nil 'dirvish--hash)
         (pcase-dolist (`(,file ,h-name ,h-fn) dirvish-hook-alist)
           (when (require file nil t) (add-hook h-name h-fn)))
         (set-frame-parameter nil 'dirvish--hash (make-hash-table :test 'equal)))
       (puthash (dv-name dv) dv (dirvish-hash))
       ,(when args `(save-excursion ,@args)) ; Body form given
       dv)))

(defmacro dirvish-kill (dv &rest body)
  "Kill a dirvish instance DV and remove it from `dirvish-hash'.
DV defaults to current dirvish instance if not given.  If BODY is
given, it is executed to unset the window configuration brought
by this instance."
  (declare (indent defun))
  `(unwind-protect
       (let ((conf (dv-window-conf ,dv)))
         (when (and (not (dirvish-dired-p ,dv)) (window-configuration-p conf))
           (set-window-configuration conf))
         (setq dirvish--transient-dvs (delete dv dirvish--transient-dvs))
         (cl-labels ((kill-when-live (b) (and (buffer-live-p b) (kill-buffer b))))
           (mapc #'kill-when-live (dv-dired-buffers ,dv))
           (mapc #'kill-when-live (dv-preview-buffers ,dv))
           (dolist (type '(preview footer header)) (kill-when-live (dirvish--get-util-buffer ,dv type))))
         (funcall (dv-quit-window-fn ,dv) ,dv))
     (remhash (dv-name ,dv) (dirvish-hash))
     (dirvish-reclaim)
     ,@body))

(defun dirvish--end-transient (tran)
  "End transient of Dirvish instance or name TRAN."
  (cl-loop
   with hash = (dirvish-hash)
   with tran-dv = (if (dirvish-p tran) tran (gethash tran hash))
   for dv-name in (mapcar #'dv-name (hash-table-values hash))
   for dv = (gethash dv-name hash)
   for dv-tran = (dv-transient dv) do
   (when (or (eq dv-tran tran) (eq dv-tran tran-dv))
     (dirvish-kill dv))
   finally (dirvish-deactivate tran-dv)))

(defun dirvish--create-root-window (dv)
  "Create root window of DV."
  (let ((depth (dv-depth dv))
        (r-win (funcall (dv-root-window-fn dv))))
    (when (and (>= depth 0) (window-parameter r-win 'window-side))
      (setq r-win (next-window)))
    (setf (dv-root-window dv) r-win)
    r-win))

(defun dirvish--enlarge (&rest _)
  "Kill all dirvish parent windows except the root one."
  (when (dirvish-curr)
    (cl-dolist (win (dv-dired-windows (dirvish-curr)))
      (and (not (eq win (dv-root-window (dirvish-curr))))
           (window-live-p win)
           (delete-window win)))))

(defun dirvish--refresh-slots (dv)
  "Update dynamic slot values of DV."
  (when dirvish-attributes (mapc #'require dirvish-extra-libs))
  (let* ((attr-names (append dirvish--builtin-attrs (dv-raw-attributes dv)))
         (attrs-alist
          (cl-loop for name in attr-names
                   for attr = (cdr-safe (assoc name dirvish--available-attrs))
                   collect (cl-destructuring-bind (&key overlay if fn left right &allow-other-keys)
                               attr (list overlay if fn left right))))
         (preview-dps
          (cl-loop for dp-name in (append '(disable) (dv-raw-preview-dps dv) '(default))
                   for dp-func-name = (intern (format "dirvish-%s-preview-dp" dp-name))
                   collect dp-func-name))
         (scopes (cl-loop with res-plist = `(:dv ,dv)
                          for (key value) on dirvish-scopes by 'cddr
                          do (setq res-plist (append res-plist (list key (funcall value))))
                          finally return res-plist)))
    (setf (dv-attributes-alist dv) attrs-alist)
    (setf (dv-preview-dispatchers dv) preview-dps)
    (setf (dv-scopes dv) scopes)
    (unless (dirvish-dired-p dv) (setf (dv-depth dv) (dv-read-only-depth dv)))
    (setf (dv-fullscreen-depth dv) (dv-read-only-depth dv))))

(defun dirvish--render-attributes (dv)
  "Render attributes in Dirvish session DV's body."
  (when (> (length dirvish--attrs-alist) 500)
    (setq-local dirvish--attrs-alist nil))
  (let* ((attrs (dv-attributes-alist dv))
         (curr-pos (point))
         (fr-h (frame-height))
         (fns (cl-loop with (left-w . right-w) = (cons dirvish--prefix-spaces 0)
                       for (ov pred fn left right) in attrs
                       do (remove-overlays (point-min) (point-max) ov t)
                       for valid = (funcall pred dv)
                       when valid do (progn (setq left-w (+ left-w (or (eval left) 0)))
                                            (setq right-w (+ right-w (or (eval right) 0))))
                       when valid collect (prog1 fn (setq-local dirvish--attrs-width (cons left-w right-w))))))
    (save-excursion
      (forward-line (- 0 fr-h))
      (cl-dotimes (_ (* 2 fr-h))
        (when (eobp) (cl-return))
        (when-let ((f-name (dired-get-filename nil t))
                   (f-beg (and (not (invisible-p (point)))
                               (dired-move-to-filename nil)))
                   (f-end (dired-move-to-end-of-filename t)))
          (let ((f-attrs (file-attributes f-name))
                (l-beg (line-beginning-position))
                (l-end (line-end-position))
                (hl-face (and (eq f-beg curr-pos) 'dirvish-hl-line)))
            (when dirvish--dired-async-p
              (let (buffer-read-only) (dired-insert-set-properties l-beg l-end)))
            (dolist (fn fns) (funcall fn f-name f-attrs f-beg f-end l-beg l-end hl-face))))
        (forward-line 1)))))

(defun dirvish--apply-header-style ()
  "Format Dirvish header line."
  (when-let ((dv (dirvish-curr)))
    (let* ((h-fn (dv-header-string-fn dv))
           (str (format-mode-line `((:eval (funcall #',h-fn)))))
           (large-header-p (eq dirvish-header-style 'large))
           (ht (if large-header-p 1.2 1))
           (win-width (1- (* (frame-width) (- 1 dirvish-preview-width))))
           (max-width (floor (/ win-width ht))))
      (while (>= (dirvish--actual-string-length str) (1- max-width))
        (setq str (substring str 0 -1)))
      (propertize str 'display `((height ,ht) (raise ,(if large-header-p 0.25 0.35)))))))

(defun dirvish--format-mode-line ()
  "Generate Dirvish mode line string."
  (when (dirvish-curr)
    (cl-destructuring-bind (left . right) dirvish-mode-line-format
      (let ((fmt-right (format-mode-line right)))
        (concat (format-mode-line left)
                (propertize " " 'display
                            `((space :align-to (- (+ right right-fringe right-margin)
                                                  ,(string-width fmt-right)))))
                fmt-right)))))

(defun dirvish--deactivate-for-tab (tab _only-tab)
  "Deactivate all dvs in TAB."
  (dolist (scope (dirvish-get-all 'scopes t))
    (when (eq (plist-get scope :tab) (tab-bar--tab-index tab))
      (dirvish-deactivate (plist-get scope :dv)))))

(defun dirvish--deactivate-for-frame (frame)
  "Deactivate all dvs in FRAME."
  (dolist (scope (dirvish-get-all 'scopes t))
    (when (eq (plist-get scope :frame) frame)
      (dirvish-deactivate (plist-get scope :dv)))))

(defun dirvish-activate (dv)
  "Activate dirvish instance DV."
  (setq tab-bar-new-tab-choice "*scratch*")
  (when-let (old-dv (dirvish-curr))
    (cond ((dv-transient dv) nil)
          ((and (not (dirvish-dired-p old-dv))
                (not (dirvish-dired-p dv)))
           (dirvish-deactivate dv)
           (user-error "Dirvish: using current session"))
          ((memq (selected-window) (dv-dired-windows old-dv))
           (dirvish-deactivate old-dv))))
  (dirvish--refresh-slots dv)
  (dirvish--create-root-window dv)
  (set-frame-parameter nil 'dirvish--curr dv)
  (when-let ((path (dv-path dv))) (dirvish-find-file path))
  (run-hooks 'dirvish-activation-hook)
  dv)

(defun dirvish-deactivate (dv)
  "Deactivate dirvish instance DV."
  (dirvish-kill dv
    (unless (dirvish-get-all 'name t t)
      (setq other-window-scroll-buffer nil)
      (setq tab-bar-new-tab-choice dirvish--saved-new-tab-choice)
      (dolist (tm dirvish--repeat-timers) (cancel-timer (symbol-value tm)))))
  (run-hooks 'dirvish-deactivation-hook)
  (and dirvish-debug-p (message "leftover: %s" (dirvish-get-all 'name t t))))

(defun dirvish-dired-p (&optional dv)
  "Return t if DV is a `dirvish-dired' instance.
DV defaults to the current dirvish instance if not provided."
  (when-let ((dv (or dv (dirvish-curr)))) (eq (dv-depth dv) -1)))

(defun dirvish-live-p (&optional dv)
  "Return t if selected window is occupied by Dirvish DV.
DV defaults to the current dirvish instance if not provided."
  (when-let ((dv (or dv (dirvish-curr)))) (memq (selected-window) (dv-dired-windows dv))))

;;;; Advices

(defun dirvish-subtree-remove-ad (fn &rest _)
  "Advisor for FN `dired-subtree-remove'."
  (dirvish--hide-dired-header (funcall fn))) ; See `dired-hacks' #170

(defun dirvish-dired-ad (fn dirname &optional switches)
  "Override `dired' command.
FN refers to original `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when-let ((dv (dirvish-curr))) (dirvish-deactivate dv))
  (apply fn dirname (and switches (list switches)))
  (dirvish-activate (dirvish-new :depth -1))
  (when switches
    (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-window-ad (dirname &optional switches)
  "Override `dired-other-window' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when-let ((dv (dirvish-curr)))
    (unless (dirvish-dired-p dv) (dirvish-deactivate dv)))
  (switch-to-buffer-other-window (dirvish--ensure-temp-buffer))
  (dirvish-activate (dirvish-new :depth -1))
  (when switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab (dirvish--ensure-temp-buffer))
  (dirvish-drop)
  (dirvish-activate (dirvish-new :depth -1))
  (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
  (dirvish-find-file dirname))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (let (after-focus-change-function)
    (switch-to-buffer-other-frame (dirvish--ensure-temp-buffer))
    (dirvish-activate (dirvish-new :depth dirvish-depth))
    (and switches (setf (dv-ls-switches (dirvish-curr)) switches))
    (dirvish-find-file dirname)))

(defun dirvish-dired-jump-ad (_fn &optional other-window file-name)
  "An advisor for `dired-jump' command.
OTHER-WINDOW and FILE-NAME are the same args in `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg (read-file-name "Dirvish jump to: "))))
  (if (and (dirvish-curr) (not other-window))
      (dirvish-find-file file-name)
    (dirvish-dired (or file-name default-directory) other-window)))

(defun dirvish-find-dired-sentinel-ad (&rest _)
  "Advisor function for `find-dired-sentinel'."
  (let ((dv (dirvish-curr))
        (last-dv (with-current-buffer (other-buffer)
                   (when (derived-mode-p 'dirvish-mode) (dirvish-curr))))
        buffer-read-only)
    (unless (and dv (eq (dv-type dv) 'find-dired))
      (let* ((last-depth
              (with-current-buffer (other-buffer)
                (and (derived-mode-p 'dirvish-mode) (dv-depth (dirvish-curr)))))
             (new-dv (dirvish-new :type 'find-dired :dedicated t)))
        (add-to-list 'dirvish--transient-dvs new-dv)
        (setf (dv-transient new-dv) (or last-dv new-dv))
        (dirvish-activate new-dv)
        (setf (dv-depth new-dv) (or last-depth 0))
        (setq-local dirvish--curr-name (dv-name new-dv))
        (dirvish-reclaim)
        (dirvish-build)))
    ;; BUG?: `dired-move-to-filename' failed to parse filename when there is only 1 file in buffer
    (delete-matching-lines "find finished at.*\\|^ +$")
    (dirvish--hide-dired-header)
    (and (dirvish-curr) (dirvish-setup 'keep-dired))))

(defun dirvish-fd-dired-ad (fn &rest args)
  "Advisor function for FN `fd-dired' with its ARGS."
  (when (and (dirvish-curr) (eq (dv-type (dirvish-curr)) 'find-dired))
    (dirvish-deactivate (dirvish-curr)))
  ;; HACK for *FD* window placement. `fd-dired-display-in-current-window' does not behave as described.
  (let ((display-buffer-alist '(("^ ?\\*Fd.*$" (display-buffer-same-window))))
        (fd-dired-buffer-name-format "*%s*"))
    (apply fn args)))

(defun dirvish-dwim-target-next-ad (&optional all-frames)
  "Replacement for `dired-dwim-target-next'.
If ALL-FRAMES, search target directories in all frames."
  (mapcan (lambda (w)
            (when (or all-frames
                      (eq (and (window-valid-p w) (window-frame w)) (selected-frame)))
              (with-current-buffer (window-buffer w)
                (list (dired-current-directory)))))
          (delq (selected-window) (dirvish-get-all 'root-window t t))))

(defun dirvish-wdired-mode-ad (&rest _)
  "Advisor function for `wdired-change-to-wdired-mode'."
  (dired-move-to-end-of-filename t)
  (setq-local cursor-type '(bar 4))
  (dolist (ov (mapcar #'car (dv-attributes-alist (dirvish-curr))))
    (remove-overlays (point-min) (point-max) ov t))
  (remove-hook 'post-command-hook #'dirvish-update-body-h :local))

(defun dirvish-refresh-cursor-ad (fn &rest args)
  "Only apply FN with ARGS when editing filenames in dirvish."
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-deletion-ad (fn &rest args)
  "Advice function for FN `dired-internal-do-deletions' with its ARGS."
  (let ((trash-directory (dirvish--get-trash-dir))) (apply fn args)))

(defun dirvish-find-file-ad (&rest _)
  "Quit Dirvish session if inside one."
  (when-let* ((dv (dirvish-curr)))
    (if-let ((transient (dv-transient dv)))
        (dirvish--end-transient transient)
      (select-window (funcall (dv-find-file-window-fn dv)))
      (when (dirvish-live-p dv) (dirvish-deactivate dv)))))

(defun dirvish-ignore-ad (fn &rest args)
  "Only apply FN with ARGS outside of Dirvish."
  (unless (dirvish-curr) (apply fn args)))

(defun dirvish--add-advices ()
  "Add all advices listed in `dirvish-advice-alist'."
  (pcase-dolist (`(,file ,sym ,fn ,place) dirvish-advice-alist)
    (when (require file nil t) (advice-add sym (or place :around) fn))))

(defun dirvish--remove-advices ()
  "Remove all advices listed in `dirvish-advice-alist'."
  (pcase-dolist (`(,_ ,sym ,fn) dirvish-advice-alist) (advice-remove sym fn)))

;;;; Preview

(defun dirvish--preview-image-size (window &optional height)
  "Get corresponding image width or HEIGHT in WINDOW."
  (floor (* dirvish--preview-img-scale (funcall (if height #'window-pixel-height #'window-pixel-width) window))))

(defun dirvish--get-image-cache-for-file (file size &optional ext no-mkdir)
  "Get image cache filepath for FILE.
SIZE is window pixelwise width of current dirvish preview window.
A optional extension EXT, such as \".jpg\", can be given to the
cache image. A new directory is created unless NO-MKDIR."
  (let ((cache (concat dirvish-cache-dir (number-to-string size)
                       (when dirvish--os-windows-p "/")
                       (replace-regexp-in-string ":" "" file))))
    (and (not no-mkdir) (not (file-exists-p cache))
         (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish-preview--insert-image (image dv)
  "Insert IMAGE at preview window of DV."
  (insert " ")
  (add-text-properties 1 2 `(display ,image rear-nonsticky t keymap ,image-map))
  (cl-destructuring-bind (i-width . i-height) (image-size (image-get-display-property))
    (let* ((p-window (dv-preview-window dv))
           (w-offset (max (round (/ (- (window-width p-window) i-width) 2)) 0))
           (h-offset (max (round (/ (- (window-height p-window) i-height) 2)) 0)))
      (goto-char 1)
      (insert (make-string h-offset ?\n) (make-string w-offset ?\s)))))

(defun dirvish-clean-preview-images (fileset)
  "Clean image cache for FILESET."
  (let ((win (dv-preview-window (dirvish-curr))) size)
    (when (window-live-p win)
      (setq size (dirvish--preview-image-size win))
      (dolist (file fileset)
        (mapc #'delete-file (file-expand-wildcards
                             (dirvish--get-image-cache-for-file file size ".*" t) t))))))

(defun dirvish--preview-process-fill-str-sentinel (proc _exitcode)
  "A sentinel for dirvish preview process.
When PROC finishes, fill preview buffer with process result."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--get-util-buffer dv 'preview)
      (erase-buffer) (remove-overlays)
      (let ((result-str (with-current-buffer (process-buffer proc) (buffer-string)))
            (p-min (point-min)))
        (insert result-str)
        (ansi-color-apply-on-region
         p-min (progn (goto-char p-min) (forward-line (frame-height)) (point)))))))

(dirvish-define-preview disable (file)
  "Disable preview in some cases."
  (when (or (not (file-exists-p file))
            (not (file-readable-p file))
            (member (file-name-extension file) dirvish-preview-disabled-exts))
    `(info . ,(format "File %s is not readable or in the preview blacklist." file))))

(dirvish-define-preview text (file)
  "Open FILE with `find-file-noselect'."
  (when (string-match "text/" (or (mailcap-file-name-to-mime-type file) ""))
    `(buffer . ,(find-file-noselect file t nil))))

(dirvish-define-preview gif (file)
  "Display an animated image FILE."
  (when (string= (mailcap-file-name-to-mime-type file) "image/gif")
    (let ((gif-buf (find-file-noselect file t nil))
          (callback (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (image-animate (image-get-display-property)))))))
      (run-with-idle-timer 1 nil callback gif-buf)
      `(buffer . ,gif-buf))))

(dirvish-define-preview image (file dv)
  "Display a image with width of DV's preview window."
  (when (string-match "image/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width ".jpg")))
      (cond ((file-exists-p cache)
             `(image . ,(create-image cache nil nil :max-width width :max-height height)))
            ((or (< (nth 7 (file-attributes file)) dirvish--preview-img-threshold)
                 (string-prefix-p (expand-file-name dirvish-cache-dir) file))
             `(image . ,(create-image file nil nil :max-width width :max-height height)))
            (t `(image-cache . ("convert" "-resize" ,(number-to-string width) ,file ,cache)))))))

(dirvish-define-preview video (file dv)
  "Display a video thumbnail with width of DV's preview window."
  (when (string-match "video/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s" ,(number-to-string width) "-m"))))))

(dirvish-define-preview audio (file)
  "Use output of `mediainfo' shell command as preview."
  (when (string-match "audio/" (or (mailcap-file-name-to-mime-type file) ""))
    `(shell . ("mediainfo" ,file))))

(dirvish-define-preview epub (file dv)
  "Display a epub thumbnail with width of DV's preview window."
  (when (string= (file-name-extension file) "epub")
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("epub-thumbnailer" ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf-preface (file dv)
  "Display a pdf preface image with width of DV's preview window."
  (when (string= (file-name-extension file) "pdf")
    (let* ((p-win (dv-preview-window dv))
           (width (dirvish--preview-image-size p-win))
           (height (dirvish--preview-image-size p-win 'height))
           (cache (dirvish--get-image-cache-for-file file width))
           (cache-jpg (concat cache ".jpg")))
      (if (file-exists-p cache-jpg)
          `(image . ,(create-image cache-jpg nil nil :max-width width :max-height height))
        `(image-cache . ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" ,file ,cache))))))

(dirvish-define-preview pdf-tools (file)
  "Open FILE with `find-file-noselect'."
  (when (string= (file-name-extension file) "pdf")
    `(buffer . ,(find-file-noselect file t nil))))

(dirvish-define-preview archive (file)
  "Display output from corresponding unarchive shell commands."
  (cond ((string= (file-name-extension file) "zip")
         `(shell . ("zipinfo" ,file)))
        ((member (file-name-extension file) '("tar" "zst"))
         `(shell . ("tar" "-tvf" ,file)))))

(dirvish-define-preview default (file)
  "Default preview dispatcher."
  (let ((threshold (or large-file-warning-threshold 10000000))
        (filesize (file-attribute-size (file-attributes file)))
        (enable-local-variables nil))
    (cond ((file-directory-p file) ; in case user did not specify a directory dispatcher
           `(buffer . ,(dired-noselect file)))
          ((> filesize threshold) ; do not preview too large files
           `(info . ,(format "File %s is too big for literal preview." file)))
          (t
           (let ((buf (find-file-noselect file t nil)))
             (with-current-buffer buf
               (if (so-long-detected-long-line-p) ; do not preview files containing too long lines
                   (progn
                     (kill-buffer buf)
                     `(info . ,(format "File %s contains very long lines, preview skipped." file)))
                 `(buffer . ,buf))))))))

(defun dirvish-preview-dispatch (preview-type payload dv)
  "Execute dispatcher's PAYLOAD according to PREVIEW-TYPE.
This function apply the payloads provided by the first
matched preview dispatcher to the preview buffer, and finally
return the buffer.

PREVIEW-TYPE can be one of following values:

- `info', meaning PAYLOAD is a string.
- `buffer', meaning PAYLOAD is a buffer.
- `image', meaning PAYLOAD is a image.
- `image-cache', meaning PAYLOAD is a (IMAGE-CMD . ARGS) cons.
- `shell', meaning PAYLOAD is a (TEXT-CMD . ARGS) cons.

According to the PAYLOAD, one of these action is applied:

- A string/image PAYLOAD is inserted to the default preview buffer.
- A buffer PAYLOAD is used as preview buffer directly.
- A subprocess for IMAGE/TEXT-CMD is issued.  When the subprocess
finishes, the content in preview buffer is filled with the result
string of TEXT-CMD or the generated cache image of IMAGE-CMD."
  (let ((buf (dirvish--get-util-buffer (dirvish-curr) 'preview))
        (cmd (car-safe payload))
        (args (cdr-safe payload))
        (process-connection-type nil))
    (when (and (memq preview-type '(shell image-cache))
               (not (executable-find cmd)))
      (setq preview-type 'info
            payload (format "Install `%s' to preview this file." cmd)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (cl-case preview-type
        ('info (insert payload))
        ('buffer (setq buf payload))
        ('image (dirvish-preview--insert-image payload dv))
        ('image-cache
         (let ((proc (apply #'start-process "dirvish-preview-process" buf cmd args)))
           (set-process-sentinel
            proc (lambda (&rest _) (dirvish-debounce layout (dirvish-preview-update))))
           (insert " [Dirvish] Generating image cache...")))
        ('shell
         (let* ((res-buf (get-buffer-create " *Dirvish preview result*"))
                (proc (apply #'start-process "dirvish-preview-process" res-buf cmd args)))
           (with-current-buffer res-buf (erase-buffer) (remove-overlays))
           (set-process-sentinel proc 'dirvish--preview-process-fill-str-sentinel))))
      buf)))

(defun dirvish-get-preview-buffer (file)
  "Create the preview buffer for FILE."
  (and (file-directory-p file) (setq file (file-name-as-directory file)))
  (cl-loop with dv = (dirvish-curr)
           for dispatcher in (dv-preview-dispatchers dv)
           for (dv-type . payload) = (funcall dispatcher file dv)
           for buffer = (dirvish-preview-dispatch dv-type payload dv)
           until dv-type
           finally return buffer))

(defun dirvish-preview-update ()
  "Update dirvish preview."
  (when-let* ((curr-dv (dirvish-curr))
              (preview-window (dv-preview-window curr-dv)))
    (when (window-live-p preview-window)
      (let* ((orig-buffer-list (buffer-list))
             (index (or (dv-index-path curr-dv) ""))
             (preview-buffer (dirvish-get-preview-buffer index)))
        (setq other-window-scroll-buffer preview-buffer)
        (set-window-buffer preview-window preview-buffer)
        (unless (memq preview-buffer orig-buffer-list)
          (push preview-buffer (dv-preview-buffers curr-dv)))
        (with-current-buffer preview-buffer (run-hooks 'dirvish-preview-setup-hook))))))

;;;; Builder

(dirvish-define-attribute hl-line
  :form
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end)))) (overlay-put ov 'face hl-face) ov)))

;; This hack solves 2 issues:
;; 1. Hide " -> " arrow of symlink files as well.
;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
(dirvish-define-attribute symlink-target
  :if (and dired-hide-details-mode (default-value 'dired-hide-details-hide-symlink-targets))
  :form
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end))) (overlay-put ov 'invisible t) ov)))

(defun dirvish-default-header-string-fn ()
  "Compose header string."
  (when-let ((dv (dirvish-curr)))
    (let* ((index (dv-index-path dv))
           (file-path (or (file-name-directory index) ""))
           (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
           (path-regex (concat (getenv "HOME") "/\\|\\/$"))
           (path-tail (replace-regexp-in-string path-regex "" file-path))
           (file-name (file-name-nondirectory index)))
      (format " %s %s %s"
              (propertize (if path-prefix-home "~" ":"))
              (propertize path-tail 'face 'dired-mark)
              (propertize file-name 'face 'font-lock-constant-face)))))

(defun dirvish-find-dired-header-string-fn ()
  "Return a string showing current `find/fd' command args."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (when-let ((args (or (bound-and-true-p fd-dired-input-fd-args) find-args)))
      (format " %s [%s] at %s"
              (propertize "FD:" 'face 'bold)
              (propertize args 'face 'font-lock-string-face)
              (propertize default-directory 'face 'dired-header)))))

(defun dirvish--mode-line-sorter ()
  "Return a string showing current Dired file sort criteria."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (format " %s %s "
            (propertize "Sort:" 'face 'bold)
            (or (and dired-sort-inhibit (propertize "inhibited" 'face 'font-lock-warning-face))
                (propertize (car (dv-sort-criteria (dirvish-curr))) 'face 'font-lock-type-face)))))

(defun dirvish--mode-line-filter ()
  "Return a string showing active Dired file filter."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (cond ((bound-and-true-p dired-filter-mode)
           (format " %s %s " (propertize "Filters:" 'face 'bold)
                   (dired-filter--describe-filters)))
          (dired-omit-mode (propertize "[Omit]" 'face 'bold)))))

(defun dirvish--mode-line-index ()
  "Return a string showing index in a Dirvish buffer."
  (with-current-buffer (window-buffer (dv-root-window (dirvish-curr)))
    (let ((cur-pos (- (line-number-at-pos (point)) 1))
          (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
      (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold)))))

(defun dirvish-rebuild-parents-h (frame)
  "Rebuild dirvish layout in FRAME."
  (dirvish-reclaim frame)
  (when-let ((dv (and (dirvish-live-p) (dirvish-curr))))
    (unless (dv-transient dv) (dirvish-build))))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let ((dv (dirvish-curr)))
    (cond ((eobp) (forward-line -1)) ((bobp) (forward-line 1)))
    (dired-move-to-filename)
    (dirvish--render-attributes dv)
    (when-let ((filename (dired-get-filename nil t)))
      (setf (dv-index-path dv) filename)
      (dirvish-debounce layout
        (with-current-buffer (dirvish--get-util-buffer dv 'footer) (force-mode-line-update))
        (with-current-buffer (dirvish--get-util-buffer dv 'header) (force-mode-line-update))
        (dirvish-preview-update)))))

(defun dirvish-quit-h ()
  "Quit current Dirvish."
  ;; FIXME: dv should be always accessible here
  (if-let ((dv (gethash dirvish--curr-name (dirvish-hash))))
      (dirvish-deactivate dv)
    (kill-current-buffer))
  (switch-to-buffer (dirvish--ensure-temp-buffer)))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function."
  (dired-revert)
  (dirvish-clean-preview-images (dired-get-marked-files))
  (dirvish--hide-dired-header)
  (dirvish-update-body-h))

(defun dirvish-setup (&optional keep-dired)
  "Default config for dirvish parent windows.
If KEEP-DIRED is specified, reuse the old Dired buffer."
  (unless keep-dired
    (dirvish-mode)
    (setq-local revert-buffer-function #'dirvish-revert)
    (dirvish--hide-dired-header))
  (set (make-local-variable 'face-remapping-alist) dirvish-face-remap-alist)
  (setq-local face-font-rescale-alist nil)
  (setq-local dired-hide-details-hide-symlink-targets nil) ;; See `dirvish--render-symlink-target'
  (setq cursor-type nil)
  (set-window-fringes nil 1 1)
  (when (bound-and-true-p all-the-icons-dired-mode)
    (all-the-icons-dired-mode -1)
    (setq-local tab-width 2))
  (when dirvish--child-entry (dired-goto-file dirvish--child-entry))
  (setq dirvish--vc-backend (ignore-errors (vc-responsible-backend default-directory)))
  (let (dired-hide-details-mode-hook) (dired-hide-details-mode t))
  (let* ((dv (dirvish-curr))
         (owp (dirvish-dired-p dv))
         (header-fn (dv-header-string-fn dv)))
    (dirvish--render-attributes dv)
    (push (selected-window) (dv-dired-windows dv))
    (push (current-buffer) (dv-dired-buffers dv))
    (setq-local dirvish--curr-name (dv-name dv))
    (setq mode-line-format (and owp dirvish-mode-line-format '((:eval (dirvish--format-mode-line)))))
    (setq header-line-format (and owp dirvish-header-string-function `((:eval (funcall #',header-fn))))))
  (add-hook 'window-buffer-change-functions #'dirvish-rebuild-parents-h nil :local)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil :local)
  (add-hook 'quit-window-hook #'dirvish-quit-h nil :local)
  (run-hooks 'dirvish-mode-hook))

(defun dirvish--build-parents (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (depth (if (dv-dedicated dv) 0 (dv-depth dv)))
         (i 0))
    (dirvish-setup dirvish--curr-name)
    (when (window-parameter (selected-window) 'window-side)
      (setq-local window-size-fixed 'width))
    (while (and (< i depth) (not (string= current parent)))
      (setq i (1+ i))
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent current))
      (setq parent (dirvish--get-parent parent)))
    (when (> depth 0)
      (let* ((remain (- 1 dirvish-preview-width dirvish-parent-max-width))
             (width (min (/ remain depth) dirvish-parent-max-width))
             (dired-after-readin-hook nil))
        (cl-dolist (parent-dir parent-dirs)
          (let* ((current (car parent-dir))
                 (parent (cdr parent-dir))
                 (win-alist `((side . left)
                              (inhibit-same-window . t)
                              (window-width . ,width)
                              (window-parameters . ((no-other-window . t)))))
                 (buffer (dirvish--buffer-for-dir dv parent t))
                 (window (display-buffer buffer `(dirvish--display-buffer . ,win-alist))))
            (with-selected-window window
              (setq-local dirvish--child-entry current)
              (dirvish-setup)
              ;; always hide details in parent windows
              (let (dired-hide-details-mode-hook) (dired-hide-details-mode t)))))))))

(defun dirvish--build-preview (dv)
 "Create a window showing preview for DV."
  (let* ((inhibit-modification-hooks t)
         (buf (dirvish--get-util-buffer dv 'preview))
         (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
         (fringe 30)
         (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
    (set-window-fringes new-window fringe fringe nil t)
    (setf (dv-preview-window dv) new-window)))

(defun dirvish--build-header (dv)
  "Create a window showing header for DV."
  (when dirvish-header-style
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-util-buffer dv 'header))
           (win-alist `((side . above)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish--build-footer (dv)
  "Create a window showing footer for DV."
  (when dirvish-mode-line-format
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-util-buffer dv 'footer))
           (win-alist `((side . below)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish--noselect (dir)
  "Return the Dirvish buffer at DIR, do not select it."
  (or dir (setq dir default-directory))
  (let ((dv (dirvish-activate (dirvish-new :depth -1))))
    (with-current-buffer (dirvish--buffer-for-dir dv dir)
      (dirvish-build)
      (current-buffer))))

(defun dirvish--noselect-async-sentinel (proc _state)
  "Sentinel for `dirvish--noselect-async''s Dired PROC."
  (with-current-buffer (process-buffer proc)
    (let (buffer-read-only)
      (delete-region (point-min) (+ (point-min) (process-get proc 'len)))
      (delete-region (progn (goto-char (point-max)) (forward-line -1) (point)) (point-max)))
    (dirvish--hide-dired-header)
    (dirvish-debounce layout (dirvish-update-body-h))))

(defun dirvish--noselect-async (entry dired-switches)
  "Open ENTRY with DIRED-SWITCHES asynchronously."
  (with-current-buffer (generate-new-buffer "Dirvish-cache")
    (let* ((info "[DIRVISH] caching directory...\n")
           (buf (prog1 (current-buffer) (insert info)))
           (async-cmd `(with-current-buffer (dired-noselect ,entry ,dired-switches)
                         (buffer-substring-no-properties (point-min) (point-max))))
           (proc (start-process (buffer-name buf) buf "emacs" "-q" "-batch" "--eval"
                                (format "(message \"%%s\" %S)" async-cmd))))
      (setq default-directory entry)
      (setq dired-subdir-alist (list (cons entry (point-min-marker))))
      (dirvish-mode)
      (dirvish-setup t)
      (set-process-sentinel proc #'dirvish--noselect-async-sentinel)
      (process-put proc 'len (length info))
      (setq revert-buffer-function #'ignore) ; TODO
      (setq-local dirvish--dired-async-p t)
      buf)))

(defun dirvish--buffer-for-dir (dv entry &optional parent)
  "Return the root or PARENT buffer in DV for ENTRY.
If the buffer is not available, create it with `dired-noselect'."
  (let* ((dir-buf (if parent (dv-parent-dir-buf-alist dv) (dv-root-dir-buf-alist dv)))
         (buffer (alist-get entry dir-buf nil nil #'equal))
         (sorter (cdr (dv-sort-criteria dv)))
         (switches (string-join (list (dv-ls-switches dv) sorter) " ")))
    (unless buffer
      (let ((files-count (length (directory-files entry nil nil t))))
        (if (> files-count dirvish-async-listing-threshold)
            (setq buffer (dirvish--noselect-async entry switches))
          (setq buffer (dired-noselect entry switches))))
      (push (cons entry buffer)
            (if parent (dv-parent-dir-buf-alist dv) (dv-root-dir-buf-alist dv))))
    buffer))

(defun dirvish-build ()
  "Build dirvish layout."
  (let ((dv (dirvish-curr)))
    (unless (dirvish-dired-p dv)
      (let ((ignore-window-parameters t)) (delete-other-windows))
      (dirvish--build-preview dv)
      (dirvish--build-header dv)
      (dirvish--build-footer dv))
    (dirvish--build-parents dv)))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-up-directory (&optional other-window)
  "Run Dirvish on parent directory of current directory.
If OTHER-WINDOW (the optional prefix arg), display the parent
directory in another window."
  (interactive "P")
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current)))
    (if (string= parent current)
        (user-error "Dirvish: you're in root directory")
      (if other-window
          (progn
            (switch-to-buffer-other-window (dirvish--ensure-temp-buffer))
            (dirvish-activate (dirvish-new :path (dirvish--ensure-path parent) :depth -1)))
        (dirvish-find-file parent t)))))

(defun dirvish-sort-by-criteria (criteria)
  "Call `dired-sort-other' by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "Sort by (d/D)efault (e/E)xt (s/S)ize (t/T)ime (m/M)odified: "
     '(?q ?d ?D ?e ?E ?s ?S ?t ?T ?m ?M))))
  (when dired-sort-inhibit (user-error "Dirvish: cannot sort this buffer"))
  (unless (eq criteria ?q)
    (let* ((c (char-to-string criteria))
           (revp (string-equal c (upcase c)))
           (cc (downcase c))
           (sort-flag
            (cond
             ((string-equal cc "d") '("default" . ""))
             ((string-equal cc "m") '("modified" . " -c"))
             ((string-equal cc "e") '("ext" . " -X"))
             ((string-equal cc "t") '("time" . " -t"))
             ((string-equal cc "s") '("size" . " -S"))))
           (name (concat (car sort-flag) (when revp " [R]")))
           (order (concat (cdr sort-flag) (when revp " -r")))
           (dv (dirvish-curr)))
      (setf (dv-sort-criteria dv) (cons name order))
      (dired-sort-other (string-join (list (dv-ls-switches dv) order) " ")))))

(defun dirvish-toggle-fullscreen ()
  "Toggle fullscreen of current Dirvish."
  (interactive)
  (if-let* ((dv (dirvish-curr))
            (old-depth (dv-depth dv))
            (fs-depth (dv-fullscreen-depth dv))
            (new-depth (if (eq old-depth -1) fs-depth -1))
            (buf (current-buffer)))
      (progn
        (dirvish-drop)
        (if (dirvish-dired-p dv)
            (with-selected-window (dv-root-window dv)
              (let (quit-window-hook) (quit-window)))
          (set-window-configuration (dv-window-conf dv)))
        (setf (dv-depth dv) new-depth)
        (setf (dv-window-conf dv) (current-window-configuration))
        (setq-local dirvish--child-entry (dv-index-path dv))
        (with-selected-window (dirvish--create-root-window dv)
          (switch-to-buffer buf)
          (dirvish-reclaim)
          (dirvish-build)))
    (user-error "Dirvish: not in a dirvish buffer")))

(defun dirvish-find-file (&optional file ignore-hist)
  "Find file in dirvish buffer.
FILE can be a file or a directory, if nil then infer entry from
variable `buffer-file-name'.  If IGNORE-HIST is non-nil, do not
update `dirvish--history-ring'."
  (interactive)
  (let* ((entry (or file (dired-get-filename nil t)))
         (bname (buffer-file-name (current-buffer)))
         (dv (dirvish-curr))
         (dv-tran (dv-transient dv))
         (dv-depth (dv-depth dv)))
    (when entry
      (if (file-directory-p entry)
          (let* ((entry (file-name-as-directory (expand-file-name entry)))
                 (hist (directory-file-name entry))
                 enable-dir-local-variables)
            (when (and dirvish--history-ring (not ignore-hist))
              (when (or (ring-empty-p dirvish--history-ring)
                        (not (eq hist (ring-ref dirvish--history-ring 0))))
                (ring-insert dirvish--history-ring hist)))
            (switch-to-buffer (dirvish--buffer-for-dir dv entry))
            (setq dirvish--child-entry (or bname (expand-file-name default-directory)))
            (when (dirvish-p dv-tran)
              (dirvish-activate
               (dirvish-new
                 :depth dv-depth
                 :transient (dv-name dv-tran))))
            (dirvish-build))
        (find-file entry)))))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Override Dired with `dirvish-dired' globally."
  :group 'dirvish :global t
  (if dirvish-override-dired-mode
      (progn
        (dirvish--add-advices)
        (setq find-directory-functions
              (cl-substitute #'dirvish--noselect #'dired-noselect find-directory-functions)))
    (dirvish--remove-advices)
    (setq find-directory-functions
          (cl-substitute #'dired-noselect #'dirvish--noselect find-directory-functions))))

;;;###autoload
(defun dirvish (&optional path)
  "Start a full frame Dirvish session with optional PATH.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to variable `buffer-file-name'."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish: "))))
  (dirvish-activate (dirvish-new :path (dirvish--ensure-path path) :depth dirvish-depth)))

;;;###autoload
(defun dirvish-dired (&optional path other-window)
  "Start a Dirvish session with optional PATH in current window.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to variable `buffer-file-name'.  Execute it
in other window when OTHER-WINDOW is non-nil."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish dired: ")) nil))
  (and other-window (switch-to-buffer-other-window (dirvish--ensure-temp-buffer)))
  (dirvish-activate (dirvish-new :path (dirvish--ensure-path path) :depth -1)))

(provide 'dirvish)
;;; dirvish.el ends here
