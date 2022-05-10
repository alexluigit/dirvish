;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2022 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.2.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

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

(require 'so-long)
(require 'mailcap)
(require 'image-mode)
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
holds a function that takes current filename and preview window
as arguments and gets called at runtime.  It controls how the
preview content for certain filetypes are generated, or it can
decline to handle the file name and leaving it for future
dispatchers.  If none of the dispatchers can handle the preview,
the fallback dispatcher named `default' is used.  For details see
`dirvish-preview-dispatch'."
  :group 'dirvish :type 'hook)

(defcustom dirvish-preview-disabled-exts
  '("iso" "bin" "exe" "gpg" "elc" "eln")
  "Do not preview files end with these extensions."
  :group 'dirvish :type 'list)

(defvar dirvish--auto-cache-timer nil)
(defcustom dirvish-auto-cache-threshold '(500 . 4)
  "Generate cache images automatically.
The value should be a cons cell (FILES . PROCS).  Directories
that include number of files less than FILES are cached
automatically, set it to 0 disables auto caching.  PROCS is the
max number of cache processes."
  :group 'dirvish
  :type '(cons (integer :tag "Max number of directory files")
               (integer :tag "Max number of cache process"))
  :set (lambda (k v)
         (set k v)
         (and (timerp dirvish--auto-cache-timer)
              (cancel-timer dirvish--auto-cache-timer))
         (unless (eq (car v) 0)
           (setq dirvish--auto-cache-timer
                 (run-with-timer 0 0.25 #'dirvish--autocache)))))

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
(defcustom dirvish-history-length 50
  "Length of directory visiting history Dirvish will track.
Set it to nil disables the history tracking."
  :group 'dirvish :type 'integer
  :set (lambda (k v) (set k v) (setq dirvish--history-ring (make-ring v))))

(defcustom dirvish-depth 1
  "Level of directories to traverse up."
  :group 'dirvish :type 'integer)

(defcustom dirvish-parent-max-width 0.1
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-preview-width 0.6
  "Fraction of frame width taken by preview window."
  :group 'dirvish :type 'float)

(defcustom dirvish-header-string-function 'dirvish-default-header-string
  "Function that returns content (a string) in Dirvish header."
  :group 'dirvish :type 'function)

(defface dirvish-hl-line
  '((((class color) (background light)) :background "#8eecf4" :extend t)
    (((class color) (background dark)) :background "#004065" :extend t))
  "Face for Dirvish line highlighting."
  :group 'dirvish)

(defcustom dirvish-header-height '(1 . 1.15)
  "Height of text in header string.
The value should be a cons cell (H-DIRED . H-DIRVISH), where
H-DIRED and H-DIRVISH represent the text height of header in
single window session and fullscreen session respectively.  If
H-DIRVISH is 0, don't create the header window."
  :group 'dirvish
  :type '(cons (float :tag "Header text height when `dirvish-dired-p'")
               (float :tag "Header text height unless `dirvish-dired-p'")))

(defun dirvish--mode-line-fmt-setter (fmt &optional header)
  "Compose the `mode-line-format' or header-line (if HEADER) from FMT."
  (cl-labels ((expand (part)
                (cl-loop for s in (plist-get fmt part) collect
                         `(:eval (,(intern (format "dirvish-%s-ml" s)) dv))))
              (geth (&optional large)
                (funcall (if large #'cdr #'car) dirvish-header-height)))
    `((:eval
       (let* ((dv (dirvish-curr))
              (buf (window-buffer (dv-root-window dv)))
              (height ,(if header `(if (dirvish-dired-p dv) ,(geth) ,(geth t)) (geth)))
              (str-right
               (propertize (format-mode-line ',(or (expand :right)) nil nil buf)
                           'display `((height ,height)))))
         (concat
          ,(when header `(format-mode-line '(:eval (dirvish-bar-ml dv))))
          (propertize (format-mode-line
                       ',(or (expand :left) mode-line-format) nil nil buf)
                      'display `((height ,height)))
          (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(ceiling (* height (string-width str-right)))))))
          str-right))))))

(defcustom dirvish-mode-line-format
  '(:left (sort omit) :right (free-space index))
  "Mode line SEGMENTs aligned to left/right respectively.
The SEGMENTs are defined by `dirvish-define-mode-line'.
Set it to nil to use the default `mode-line-format'."
  :group 'dirvish :type 'plist
  :set (lambda (k v) (set k (dirvish--mode-line-fmt-setter v))))

(defcustom dirvish-header-line-format
  '(:left (path) :right ())
  "Like `dirvish-mode-line-format', but for header line ."
  :group 'dirvish :type 'plist
  :set (lambda (k v) (set k (dirvish--mode-line-fmt-setter v t))))

(defcustom dirvish-enabled-features-on-remote '()
  "Enabled Dirvish FEATUREs on remote hosts.
The value is a list of FEATUREs, each designated by a symbol.
The default (a nil value or an empty list) denotes the most
minimalistic UI, only line highlighting is applied.  If `extras'
is present, all attributes defined in `dirvish-extras' library
are displayed on remote hosts like in localhost.  The symbol `vc'
do similar things, but for `dirvish-vc' library.  If you have
slow ssh connection, you should leave this to nil."
  :group 'dirvish
  :type '(set :tag "Features"
              (choice (const :tag "Show attributes" extras)
                      (const :tag "VC attributes/preview-handlers" vc))))

(defcustom dirvish-hide-details t
  "Whether to hide detailed information on session startup.
The value can be a boolean or a function that takes current
Dirvish session as its argument."
  :group 'dirvish :type '(choice (const :tag "Always hide details" t)
                                 (const :tag "Never hide details" nil)
                                 (function :tag "Custom function")))

(defvar dirvish-preview-setup-hook nil
  "Hook functions for preview buffer initialization.")

(defvar dirvish-activation-hook nil
  "Hook runs after activation of a Dirvish session.")

(defvar dirvish-deactivation-hook nil
  "Hook runs after deactivation of a Dirvish session.")

;;;; Internal variables

(defvar dirvish-advice-alist
  '((dired         dired                           dirvish-dired-ad               :override)
    (dired         dired-jump                      dirvish-dired-jump-ad          :override)
    (dired         dired-find-file                 dirvish-find-file              :override)
    (dired         dired-find-alternate-file       dirvish-find-file              :override)
    (dired         dired-other-window              dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab                 dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame               dirvish-dired-other-frame-ad   :override)
    (dired         dired-up-directory              dirvish-up-directory           :override)
    (dired         +dired/quit-all                 quit-window                    :override)
    (dired-aux     dired-dwim-target-next          dirvish-dwim-target-next-ad    :override)
    (wdired        wdired-change-to-wdired-mode    dirvish-wdired-mode-ad         :after)
    (wdired        wdired-exit                     dirvish-setup                  :after)
    (wdired        wdired-finish-edit              dirvish-setup                  :after)
    (wdired        wdired-abort-changes            dirvish-setup                  :after)
    (find-dired    find-dired-sentinel             dirvish-find-dired-sentinel-ad :after)
    (files         find-file                       dirvish-find-file-ad           :filter-args)
    (dired-subtree dired-subtree-remove            dirvish-subtree-remove-ad)
    (fd-dired      fd-dired                        dirvish-fd-dired-ad)
    (recentf       recentf-track-opened-file       dirvish-ignore-ad)
    (recentf       recentf-track-closed-file       dirvish-ignore-ad)
    (winner        winner-save-old-configurations  dirvish-ignore-ad)
    (evil          evil-refresh-cursor             dirvish-ignore-ad)
    (meow          meow--update-cursor             dirvish-ignore-ad)
    (flycheck      flycheck-buffer                 dirvish-ignore-ad)
    (lsp-mode      lsp-deferred                    dirvish-ignore-ad)))
(defvar dirvish-scopes '(:tab tab-bar--current-tab-index :frame selected-frame))
(defvar dirvish-hook-alist
  '((emacs window-selection-change-functions dirvish-reclaim)
    (emacs tab-bar-tab-pre-close-functions   dirvish--deactivate-for-tab)
    (emacs delete-frame-functions            dirvish--deactivate-for-frame)))
(defvar dirvish-debug-p nil)
(defvar dirvish-override-dired-mode nil)
(defvar dirvish-extra-libs '(dirvish-extras dirvish-vc))
(defvar fd-dired-generate-random-buffer)
(defconst dirvish--prefix-spaces 2)
(defconst dirvish--debouncing-delay 0.02)
(defconst dirvish--cache-img-threshold (* 1024 1024 0.4))
(defconst dirvish--preview-img-scale 0.92)
(defconst dirvish--saved-new-tab-choice tab-bar-new-tab-choice)
(defconst dirvish--saved-window-combination-resize window-combination-resize)
(defconst dirvish--builtin-attrs '(hl-line symlink-target))
(defconst dirvish--os-windows-p (memq system-type '(windows-nt ms-dos)))
(defconst dirvish--subtree-prefix-len
  (condition-case nil (length (or (bound-and-true-p dired-subtree-line-prefix) "  ")) (error 2)))
(defconst dirvish--cache-embedded-video-thumb
  (string-match "prefer embedded image" (shell-command-to-string "ffmpegthumbnailer -h")))
(defconst dirvish--cache-img-fns
  (cl-loop for dp in '(image video epub)
           collect (intern (format "dirvish-%s-preview-dp" dp))))
(defvar dirvish--hash (make-hash-table))
(defvar dirvish--available-attrs '())
(defvar dirvish--cache-pool '())
(defvar-local dirvish--props (make-hash-table :size 10))
(defvar-local dirvish--attrs-width `(,dirvish--prefix-spaces . 0))
(defvar-local dirvish--attrs-hash nil)
(put 'dired-subdir-alist 'permanent-local t)

;;;; Helpers

(defmacro dirvish-prop (prop &rest body)
  "Retrive PROP from `dirvish--props'.
Set the PROP with BODY if given."
  (declare (indent defun))
  `(let ((v (gethash ,prop dirvish--props)))
     ,(if body `(prog1 (setq v ,@body) (puthash ,prop v dirvish--props)) `v)))

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

(defmacro dirvish-with-no-dedication (&rest body)
  "Run BODY after undedicating window."
  (declare (debug (&rest form)))
  `(progn
     (let* ((window (get-buffer-window (current-buffer)))
            (dedicated (window-dedicated-p window)))
       (set-window-dedicated-p window nil)
       ,@body
       (set-window-dedicated-p window dedicated))))

(defun dirvish-apply-ansicolor-h (_win pos)
  "Update dirvish ansicolor in preview window from POS."
  (with-current-buffer (current-buffer)
    (ansi-color-apply-on-region
     pos (progn (goto-char pos) (forward-line (frame-height)) (point)))))

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
         (ignore-window-parameters t)
         (new-window (split-window-no-error nil size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun dirvish--ensure-temp-buffer (&optional type)
  "Return a temporary buffer with optional TYPE."
  (get-buffer-create (format " *Dirvish-%s*" (or type "temp"))))

(defun dirvish--get-project-root ()
  "Get root path of current project."
  (when-let ((pj (project-current)))
    (car (with-no-warnings (project-roots pj)))))

(defun dirvish--get-parent (path)
  "Get parent directory of PATH."
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun dirvish--actual-string-length (string)
  "Get STRING's actual display length."
  (/ (+ (length string) (string-bytes string)) 2))

(defun dirvish--get-subtree-depth ()
  "Get subtree depth at point."
  (let ((dps (cl-loop for ov in (overlays-at (point)) collect
                      (or (overlay-get ov 'dired-subtree-depth) 0))))
    (or (and dps (apply #'max dps)) 0)))

(defun dirvish--subtree-expanded-p ()
  "70x Faster version of `dired-subtree--is-expanded-p'."
  (save-excursion (< (dirvish--get-subtree-depth)
                     (progn (forward-line 1) (dirvish--get-subtree-depth)))))

(defun dirvish--get-filesize (fileset)
  "Return file size of FILESET in bytes."
  (cl-labels ((f-name (f) (if (file-directory-p f)
                              (directory-files-recursively f ".*" nil t)
                            f))
              (f-size (f) (file-attribute-size (file-attributes f))))
    (cl-reduce #'+ (mapcar #'f-size (flatten-tree (mapcar #'f-name fileset))))))

(defun dirvish--append-metadata (metadata completions)
  "Append METADATA for minibuffer COMPLETIONS."
  (let ((entry (if (functionp metadata)
                   `(metadata (annotation-function . ,metadata))
                 `(metadata (category . ,metadata)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

(defun dirvish--marked-files (&optional range)
  "Get all marked filenames in RANGE.
RANGE can be `buffer', `session', `frame', `all'."
  (setq range (or range 'buffer))
  (cl-loop
   with case-fold-search = nil
   with regexp = (dired-marker-regexp)
   with buffers = (pcase range
                    ('buffer (list (current-buffer)))
                    ('session (dv-dired-buffers (dirvish-curr)))
                    ('frame (dirvish-get-all 'dired-buffers nil t))
                    ('all (dirvish-get-all 'dired-buffers t t)))
   for buffer in (seq-filter #'buffer-live-p buffers) append
   (with-current-buffer buffer
     (when (save-excursion (goto-char (point-min))
                           (re-search-forward regexp nil t))
       (dired-map-over-marks (dired-get-filename) nil)))))

;;;; Core

(defun dirvish-curr (&optional frame)
  "Get current Dirvish session in FRAME (defaults to selected)."
  (or (dirvish-prop :dv) (frame-parameter frame 'dirvish--curr)))

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
    (add-hook 'window-scroll-functions #'dirvish-apply-ansicolor-h nil t))
  (dirvish--get-util-buffer dv 'header
    (setq-local cursor-type nil)
    (setq-local header-line-format nil)
    (setq-local window-size-fixed 'height)
    (setq-local face-font-rescale-alist nil)
    (setq-local mode-line-format (dv-header-line-format dv))))

(defun dirvish-reclaim (&optional _frame-or-window)
  "Reclaim current dirvish."
  (let ((old-dv (dirvish-curr))
        (new-dv (and (dirvish-prop :dv))))
    (cond ((or (active-minibuffer-window)
               (and old-dv (eq (frame-selected-window)
                               (dv-preview-window old-dv)))))
          ((and old-dv (string-match " ?*F\\(in\\)?d.**" (buffer-name)))
           (unless (dirvish-dired-p old-dv)
             (setq other-window-scroll-buffer
                   (window-buffer (dv-preview-window old-dv)))))
          (new-dv
           (setq tab-bar-new-tab-choice "*scratch*")
           (setq window-combination-resize nil) ; avoid transient menu mess up the layout
           (unless (dirvish-dired-p new-dv)
             (setq other-window-scroll-buffer
                   (window-buffer (dv-preview-window new-dv))))
           (dirvish--init-util-buffers new-dv)
           (dirvish--add-advices)
           (set-frame-parameter nil 'dirvish--curr new-dv))
          (t
           (setq tab-bar-new-tab-choice dirvish--saved-new-tab-choice)
           (setq window-combination-resize dirvish--saved-window-combination-resize)
           (setq other-window-scroll-buffer nil)
           (dirvish--remove-advices
            (and dirvish-override-dired-mode '(dired find-dired fd-dired)))
           (set-frame-parameter nil 'dirvish--curr new-dv)))))

(cl-defmacro dirvish-define-attribute (name &key if form left right doc)
  "Define a Dirvish attribute NAME.
An attribute contains a pair of predicate/rendering functions
that are being called on `post-command-hook'.  The predicate fn
takes current session DV as argument and execute IF once.  When
IF evaluates to t, the rendering fn runs FORM for every line with
following arguments:

- `f-name'  from `dired-get-filename'
- `f-attrs' from `file-attributes'
- `f-type'  from `file-directory-p' ('dir or 'file)
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
         (args '(f-name f-attrs f-type f-beg f-end l-beg l-end hl-face))
         (pred-body (if (> (length if) 0) if t)))
    `(progn
       (add-to-list
        'dirvish--available-attrs
        (cons ',name '(:doc ,doc :left ,left :right ,right :overlay ,ov :if ,pred :fn ,render)))
       (defun ,pred (dv) (ignore dv) ,pred-body)
       (defun ,render ,args
         (ignore ,@args)
         (let ((ov ,form)) (and ov (overlay-put ov ',ov t)))))))

(defmacro dirvish-attribute-cache (file attribute &rest body)
  "Get FILE's ATTRIBUTE from `dirvish--attrs-hash'.
When the attribute does not exist, set it with BODY."
  (declare (indent defun))
  `(let* ((hash (gethash ,file dirvish--attrs-hash))
          (cached (plist-get hash ,attribute))
          (attr (or cached ,@body)))
     (unless cached
       (puthash ,file (append hash (list ,attribute attr)) dirvish--attrs-hash))
     attr))

(cl-defmacro dirvish-define-preview (name arglist &optional docstring &rest body)
  "Define a Dirvish preview dispatcher NAME.
A dirvish preview dispatcher is a function consumed by
`dirvish-preview-dispatch' which optionally takes
`file' (filename under the cursor) and `dv' (current Dirvish
session) as argument specified in ARGLIST.  DOCSTRING and BODY is
the docstring and body for this function."
  (declare (indent defun) (doc-string 3))
  (let* ((dp-name (intern (format "dirvish-%s-preview-dp" name)))
         (default-arglist '(file preview-window))
         (ignore-list (cl-set-difference default-arglist arglist)))
    `(progn (defun ,dp-name ,default-arglist ,docstring (ignore ,@ignore-list) ,@body))))

(cl-defmacro dirvish-define-mode-line (name &optional docstring &rest body)
  "Define a mode line segment NAME with BODY and DOCSTRING."
  (declare (indent defun) (doc-string 2))
  (let ((ml-name (intern (format "dirvish-%s-ml" name))))
    `(defun ,ml-name (dv) ,docstring (ignore dv) ,@body)))

(defun dirvish-get-all (slot &optional all-frame flatten)
  "Gather slot value SLOT of all Dirvish in `dirvish--hash'.
If ALL-FRAME is non-nil, collect for all frames.
If FLATTEN is non-nil, collect them as a flattened list."
  (cl-loop
   with dv-slot = (intern (format "dv-%s" slot))
   with h-vals = (hash-table-values dirvish--hash)
   with s-vals = (mapcar dv-slot h-vals)
   for h-val in h-vals
   when (or all-frame (eq (plist-get (dv-scopes h-val) :frame)
                          (selected-frame)))
   for s-val in s-vals
   if flatten append (delete-dups (flatten-tree s-val))
   else collect s-val))

(cl-defstruct (dirvish (:conc-name dv-))
  "Define dirvish data type."
  (path nil :documentation "is the initial directory.")
  (depth -1 :documentation "Actual `dirvish-depth'.")
  (fullscreen-depth dirvish-depth :documentation "Actual fullscreen `dirvish-depth'.")
  (no-parents nil :documentation "If t, do not display parents, has highest privilege.")
  (transient nil :documentation "Whether overlapping sessions is allowed or not.")
  (attributes (purecopy dirvish-attributes) :documentation "Actual `dirvish-attributes'.")
  (attributes-alist () :documentation "Render functions expanded from ATTRIBUTES.")
  (preview-dispatchers (purecopy dirvish-preview-dispatchers)
                       :documentation "Preview dispatchers in this session.")
  (preview-dispatcher-fns () :documentation "Preview functions expanded from PREVIEW-DISPATCHERS.")
  (ls-switches dired-listing-switches
               :documentation "is the listing switches passed to `dired-sort-other'.")
  (header-line-format dirvish-header-line-format :documentation "Header line format.")
  (mode-line-format dirvish-mode-line-format :documentation "Mode line format.")
  (root-window-fn (lambda (_dv) (frame-selected-window))
                  :documentation "is the function to create ROOT-WINDOW.")
  (root-window nil :documentation "is the main window created by ROOT-WINDOW-FN.")
  (find-file-window-fn #'selected-window
                       :documentation "determines the target window for `find-file'.")
  (quit-window-fn #'ignore :documentation "a function being called on `quit-window'.")
  (scopes () :documentation "Environments of this session.")
  (dired-buffers () :documentation "holds all dired buffers in this instance.")
  (preview-buffers () :documentation "holds all file preview buffers in this instance.")
  (preview-window nil :documentation "is the window to display preview buffer.")
  (name (cl-gensym) :documentation "is an unique symbol for every instance.")
  (window-conf (current-window-configuration) :documentation "saved window configuration.")
  (root-dir-buf-alist () :documentation "list of INDEX-DIR and its corresponding buffer.")
  (parent-dir-buf-alist () :documentation "Similar to ROOT-DIR-BUF-ALIST, but for parent windows.")
  (index-dir "" :documentation "is the `default-directory' in ROOT-WINDOW."))

(defmacro dirvish-new (kill-old &rest args)
  "Create a new dirvish struct and put it into `dirvish--hash'.
ARGS is a list of keyword arguments followed by an optional BODY.
The keyword arguments set the fields of the dirvish struct.
If BODY is given, it is executed to set the window configuration
for the dirvish.
When KILL-OLD is non-nil, avoid overlapping sessions.
Save point, and current buffer before executing BODY, and then
restore them after."
  (declare (indent defun))
  (let ((keywords))
    (while (keywordp (car args))
      (dotimes (_ 2) (push (pop args) keywords)))
    (setq keywords (reverse keywords))
    `(let ((old (dirvish-curr))
           (new (make-dirvish ,@keywords)))
       (unless (hash-table-keys dirvish--hash)
         (pcase-dolist (`(,file ,h-name ,h-fn) dirvish-hook-alist)
           (when (require file nil t) (add-hook h-name h-fn))))
       (puthash (dv-name new) new dirvish--hash)
       (dirvish--refresh-slots new)
       (dirvish--create-root-window new)
       (when (and old ,kill-old (eq (dv-root-window old) (dv-root-window new)))
         (unless (or (dirvish-dired-p old) (dirvish-dired-p new))
           (dirvish-kill new)
           (user-error "Dirvish: using existed session"))
         (dirvish-kill old))
       (set-frame-parameter nil 'dirvish--curr new)
       (when-let ((path (dv-path new)))
         (dirvish-find-file (expand-file-name (file-name-directory path))))
       (run-hooks 'dirvish-activation-hook)
       ,(when args `(save-excursion ,@args)) ; Body form given
       new)))

(defun dirvish-kill (dv)
  "Kill a dirvish instance DV and remove it from `dirvish--hash'.
DV defaults to current dirvish instance if not given."
  (unwind-protect
      (let ((conf (dv-window-conf dv)))
        (when (and (not (dirvish-dired-p dv)) (window-configuration-p conf))
          (set-window-configuration conf))
        (cl-labels ((kill-when-live (b) (and (buffer-live-p b) (kill-buffer b))))
          (mapc #'kill-when-live (dv-dired-buffers dv))
          (mapc #'kill-when-live (dv-preview-buffers dv))
          (dolist (type '(preview header))
            (kill-when-live (dirvish--get-util-buffer dv type))))
        (funcall (dv-quit-window-fn dv) dv))
    (remhash (dv-name dv) dirvish--hash)
    (dirvish-reclaim)
    (run-hooks 'dirvish-deactivation-hook)
    (and dirvish-debug-p (message "leftover: %s" (dirvish-get-all 'name t t)))))

(defun dirvish--create-root-window (dv)
  "Create root window of DV."
  (let ((win (funcall (dv-root-window-fn dv) dv)))
    (setf (dv-root-window dv) win)
    win))

(defun dirvish--refresh-slots (dv)
  "Update dynamic slot values of DV."
  (when dirvish-attributes (mapc #'require dirvish-extra-libs))
  (let* ((attr-names (append dirvish--builtin-attrs (dv-attributes dv)))
         (attrs-alist
          (cl-loop
           for name in attr-names
           for attr = (cdr (assoc name dirvish--available-attrs)) collect
           (cl-destructuring-bind
               (&key overlay if fn left right &allow-other-keys)
               attr (list overlay if fn left right))))
         (dp-names
          (append '(remote disable) (dv-preview-dispatchers dv) '(default)))
         (preview-dispatcher-fns
          (cl-loop for dp-name in dp-names collect
                   (intern (format "dirvish-%s-preview-dp" dp-name))))
         (scopes (cl-loop
                  with res-plist = `(:dv ,dv)
                  for (key value) on dirvish-scopes by 'cddr do
                  (setq res-plist (append res-plist (list key (funcall value))))
                          finally return res-plist)))
    (setf (dv-attributes-alist dv) attrs-alist)
    (setf (dv-preview-dispatcher-fns dv) preview-dispatcher-fns)
    (setf (dv-scopes dv) scopes)))

(defun dirvish--render-attributes (dv)
  "Render attributes in Dirvish session DV's body."
  (let* ((get-meta-p (or (not (dirvish-prop :remote))
                         (memq 'extras dirvish-enabled-features-on-remote)))
         (attrs (dv-attributes-alist dv))
         (curr-pos (point))
         (fr-h (frame-height))
         (fns (cl-loop with (left-w . right-w) = (cons dirvish--prefix-spaces 0)
                       for (ov pred fn left right) in attrs
                       do (remove-overlays (point-min) (point-max) ov t)
                       for valid = (funcall pred dv)
                       when valid do (progn (setq left-w (+ left-w (or (eval left) 0)))
                                            (setq right-w (+ right-w (or (eval right) 0))))
                       when valid collect
                       (prog1 fn (setq-local dirvish--attrs-width (cons left-w right-w))))))
    (save-excursion
      (forward-line (- 0 fr-h))
      (cl-dotimes (_ (* 2 fr-h))
        (when (eobp) (cl-return))
        (when-let ((f-name (dired-get-filename nil t))
                   (f-beg (and (not (invisible-p (point)))
                               (dired-move-to-filename nil)))
                   (f-end (dired-move-to-end-of-filename t)))
          (let ((f-attrs (and get-meta-p (dirvish-attribute-cache f-name :builtin
                                           (file-attributes f-name))))
                (f-type (and get-meta-p (dirvish-attribute-cache f-name :dir-p
                                          (if (file-directory-p f-name) 'dir 'file))))
                (l-beg (line-beginning-position))
                (l-end (line-end-position))
                (hl-face (and (eq f-beg curr-pos) 'dirvish-hl-line)))
            (when (dirvish-prop :async-p)
              (let (buffer-read-only) (dired-insert-set-properties l-beg l-end)))
            (dolist (fn fns)
              (funcall fn f-name f-attrs f-type f-beg f-end l-beg l-end hl-face))))
        (forward-line 1)))))

(defun dirvish--deactivate-for-tab (tab _only-tab)
  "Deactivate all Dirvish sessions in TAB."
  (dolist (scope (dirvish-get-all 'scopes))
    (when (eq (plist-get scope :tab) (tab-bar--tab-index tab))
      (dirvish-kill (plist-get scope :dv)))))

(defun dirvish--deactivate-for-frame (frame)
  "Deactivate all dvs in FRAME."
  (dolist (scope (dirvish-get-all 'scopes t))
    (when (eq (plist-get scope :frame) frame)
      (dirvish-kill (plist-get scope :dv)))))

(defun dirvish-dired-p (&optional dv)
  "Return t if DV is a `dirvish-dired' instance.
DV defaults to the current dirvish instance if not provided."
  (when-let ((dv (or dv (dirvish-curr)))) (eq (dv-depth dv) -1)))

;;;; Advices

(defun dirvish-subtree-remove-ad (fn &rest _)
  "Advisor for FN `dired-subtree-remove'."
  (dirvish--hide-dired-header (funcall fn))) ; See `dired-hacks' #170

(defun dirvish-dired-ad (dirname &optional switches)
  "Override `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (dirvish-new t :path dirname :depth -1 :ls-switches switches))

(defun dirvish-dired-other-window-ad (dirname &optional switches)
  "Override `dired-other-window' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when-let ((dv (dirvish-curr)))
    (unless (dirvish-dired-p dv) (dirvish-kill dv)))
  (switch-to-buffer-other-window (dirvish--ensure-temp-buffer))
  (dirvish-new t :path dirname :depth -1 :ls-switches switches))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab (dirvish--ensure-temp-buffer))
  (dirvish-new t :path dirname :ls-switches switches))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (let (after-focus-change-function)
    (switch-to-buffer-other-frame (dirvish--ensure-temp-buffer))
    (dirvish-new t :path dirname :ls-switches switches :depth dirvish-depth)))

(defun dirvish-dired-jump-ad (&optional other-window file-name)
  "Override `dired-jump' command.
OTHER-WINDOW and FILE-NAME are the same args in `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg (read-file-name "Dirvish jump to: "))))
  (if (and (dirvish-curr) (not other-window))
      (dirvish-find-file file-name)
    (dirvish-dired (or file-name default-directory) other-window)))

(defun dirvish-find-dired-sentinel-ad (&rest _)
  "Advice function for `find-dired-sentinel'."
  (let ((dv (or (dirvish-curr) (dirvish-new nil)))
        (dirname-str (format "DIRVISH-FD@%s" (dired-current-directory)))
        buffer-read-only)
    (setq-local dirvish--props (make-hash-table :size 10))
    (dirvish-prop :child (or (dired-get-filename nil t) "."))
    (dirvish-prop :dv dv)
    (dirvish-prop :fd-dir dirname-str)
    (setf (dv-no-parents dv) t)
    (setf (dv-header-line-format dv)
          (dirvish--mode-line-fmt-setter '(:left (find-dired)) t))
    ;; BUG?: `dired-move-to-filename' failed to parse filename when there is only 1 file in buffer
    (delete-matching-lines "find finished at.*\\|^ +$")
    (dirvish--hide-dired-header)
    (dirvish--init-util-buffers dv)
    (push (cons dirname-str (current-buffer))
          (dv-root-dir-buf-alist dv))
    (dirvish-build dv)))

(defun dirvish-fd-dired-ad (fn &rest args)
  "Advice function for FN `fd-dired' with its ARGS."
  ;; HACK: `fd-dired-display-in-current-window' does not behave as described.
  (let ((display-buffer-alist
         '(("^ ?\\*Fd.*$" (display-buffer-same-window))))
        (fd-dired-generate-random-buffer t))
    (apply fn args)))

(defun dirvish-dwim-target-next-ad (&optional all-frames)
  "Replacement for `dired-dwim-target-next'.
If ALL-FRAMES, search target directories in all frames."
  (delete (dired-current-directory) (dirvish-get-all 'index-dir all-frames t)))

(defun dirvish-wdired-mode-ad (&rest _)
  "Advisor function for `wdired-change-to-wdired-mode'."
  (dired-move-to-end-of-filename t)
  (setq-local cursor-type '(bar 4))
  (dolist (ov (mapcar #'car (dv-attributes-alist (dirvish-curr))))
    (remove-overlays (point-min) (point-max) ov t))
  (remove-hook 'post-command-hook #'dirvish-update-body-h t))

(defun dirvish-find-file-ad (args)
  "Advice for `find-file' with its ARGS."
  (when-let ((dv (dirvish-curr)))
    (cond ((> (length (get-buffer-window-list nil nil t)) 1)
           (switch-to-buffer "*scratch*")
           (dirvish-drop)
           (when (string= (car args) "") (setf (car args) (dv-index-dir dv))))
          (t (select-window (funcall (dv-find-file-window-fn dv)))
             (when-let ((dv (dirvish-prop :dv))) (dirvish-kill dv)))))
  args)

(defun dirvish-ignore-ad (fn &rest args)
  "Only apply FN with ARGS outside of Dirvish."
  (when (or (not (dirvish-curr)) (derived-mode-p 'wdired-mode))
    (apply fn args)))

(defun dirvish--add-advices (&optional packages)
  "Add all advices listed in `dirvish-advice-alist'.
If PACKAGES, only add advices for these packages."
  (when packages (dolist (package packages) (require package)))
  (pcase-dolist (`(,pkg ,sym ,fn ,place) dirvish-advice-alist)
    (when (or (and packages (memq pkg packages))
              (and (not packages) (require pkg nil t)))
      (advice-add sym (or place :around) fn))))

(defun dirvish--remove-advices (&optional exclude-packages)
  "Remove all advices listed in `dirvish-advice-alist'.
If EXCLUDE-PACKAGES, do not remove advices for these packages."
  (pcase-dolist (`(,pkg ,sym ,fn) dirvish-advice-alist)
    (unless (memq pkg exclude-packages) (advice-remove sym fn))))

;;;; Preview

(defun dirvish-preview--image-size (window &optional height)
  "Get corresponding image width or HEIGHT in WINDOW."
  (floor (* dirvish--preview-img-scale
            (funcall (if height #'window-pixel-height #'window-pixel-width) window))))

(defun dirvish-preview--cache-image-path (file size &optional ext no-mkdir)
  "Get image cache filepath for FILE.
SIZE is window pixelwise width of current dirvish preview window.
A optional extension EXT, such as \".jpg\", can be given to the
cache image. A new directory is created unless NO-MKDIR."
  (let ((cache (concat dirvish-cache-dir "images/" (number-to-string size)
                       (when dirvish--os-windows-p "/")
                       (replace-regexp-in-string ":" "" file))))
    (and (not no-mkdir) (not (file-exists-p cache))
         (make-directory (file-name-directory cache) t))
    (concat cache ext)))

(defun dirvish-preview--insert-image (image dv)
  "Insert IMAGE at preview window of DV."
  (insert " ")
  (add-text-properties 1 2 `(display ,image rear-nonsticky t keymap ,image-map))
  (pcase-let ((`(,i-width . ,i-height) (image-size (image-get-display-property))))
    (let* ((p-window (dv-preview-window dv))
           (w-offset (max (round (/ (- (window-width p-window) i-width) 2)) 0))
           (h-offset (max (round (/ (- (window-height p-window) i-height) 2)) 0)))
      (goto-char 1)
      (insert (make-string h-offset ?\n) (make-string w-offset ?\s)))))

(defun dirvish-preview--inhibit-long-line (file)
  "Preview FILE unless it contains long lines."
  (let* ((enable-local-variables nil)
         (vc-follow-symlinks t)
         (buf (find-file-noselect file t)))
    (with-current-buffer buf
      (if (funcall so-long-predicate)
          (progn
            (kill-buffer buf)
            `(info . ,(format "File %s contains very long lines, preview skipped." file)))
        `(buffer . ,buf)))))

(defun dirvish-preview--clean-cache-images (fileset)
  "Clean image cache for FILESET."
  (clear-image-cache)
  (let ((win (dv-preview-window (dirvish-curr))) size)
    (when (window-live-p win)
      (setq size (dirvish-preview--image-size win))
      (dolist (file fileset)
        (mapc #'delete-file (file-expand-wildcards
                             (dirvish-preview--cache-image-path file size ".*" t) t))))))

(defun dirvish-preview--fill-string-sentinel (proc _exitcode)
  "A sentinel for dirvish preview process.
When PROC finishes, fill preview buffer with process result."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--get-util-buffer dv 'preview)
      (erase-buffer) (remove-overlays)
      (let* ((proc-buf (process-buffer proc))
             (result-str (with-current-buffer proc-buf (buffer-string)))
             (p-min (point-min)))
        (with-current-buffer proc-buf (erase-buffer))
        (insert result-str)
        (ansi-color-apply-on-region
         p-min (progn (goto-char p-min) (forward-line (frame-height)) (point)))))))

(defun dirvish-preview--img-cache-sentinel (proc _exitcode)
  "Sentinel for image cache process PROC."
  (when-let* ((dv (dirvish-curr))
              (path (dirvish-prop :child)))
    (and (equal path (process-get proc 'path))
         (dirvish-debounce layout (dirvish-preview-update dv)))))

(dirvish-define-preview remote (file)
  "Preview files with `ls' or `cat' for remote files."
  (when-let ((local (file-remote-p file 'localname)))
    `(shell . ("ssh" ,(file-remote-p file 'host) "test" "-d" ,local
               "&&" "ls" "-Alh" "--group-directories-first" "" ,local
               "||" "cat" ,local))))

(dirvish-define-preview disable (file)
  "Disable preview in some cases."
  (when (or (not (file-exists-p file))
            (not (file-readable-p file))
            (member (file-name-extension file) dirvish-preview-disabled-exts))
    `(info . ,(format "Preview for %s has been disabled" file))))

(dirvish-define-preview text (file)
  "Open FILE with `find-file-noselect'."
  (when (string-match "text/" (or (mailcap-file-name-to-mime-type file) ""))
    (dirvish-preview--inhibit-long-line file)))

(dirvish-define-preview gif (file)
  "Display an animated image FILE."
  (when (string= (mailcap-file-name-to-mime-type file) "image/gif")
    (let ((gif-buf (find-file-noselect file t))
          (callback (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (image-animate (image-get-display-property)))))))
      (run-with-idle-timer 1 nil callback gif-buf)
      `(buffer . ,gif-buf))))

(dirvish-define-preview audio (file)
  "Use output of `mediainfo' command for FILE as preview."
  (when (string-match "audio/" (or (mailcap-file-name-to-mime-type file) ""))
    `(shell . ("mediainfo" ,file))))

(dirvish-define-preview image (file preview-window)
  "Display a image FILE in PREVIEW-WINDOW."
  (when (string-match "image/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((width (dirvish-preview--image-size preview-window))
           (height (dirvish-preview--image-size preview-window 'height))
           (cache (dirvish-preview--cache-image-path file width ".jpg")))
      (cond ((file-exists-p cache)
             `(image . ,(create-image cache nil nil :max-width width :max-height height)))
            ((or (< (nth 7 (file-attributes file)) dirvish--cache-img-threshold)
                 (string-prefix-p (concat (expand-file-name dirvish-cache-dir) "images/") file))
             `(image . ,(create-image file nil nil :max-width width :max-height height)))
            (t `(image-cache . ("convert" ,file "-define" "jpeg:extent=300kb" "-resize"
                                ,(number-to-string width) ,cache)))))))

(dirvish-define-preview video (file preview-window)
  "Display a video thumbnail for FILE in PREVIEW-WINDOW."
  (when (string-match "video/" (or (mailcap-file-name-to-mime-type file) ""))
    (let* ((width (dirvish-preview--image-size preview-window))
           (height (dirvish-preview--image-size preview-window 'height))
           (cache (dirvish-preview--cache-image-path file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s"
                         ,(number-to-string width)
                         ,(if dirvish--cache-embedded-video-thumb "-m" "")))))))

(dirvish-define-preview epub (file preview-window)
  "Display a epub thumbnail for FILE in PREVIEW-WINDOW."
  (when (string= (file-name-extension file) "epub")
    (let* ((width (dirvish-preview--image-size preview-window))
           (height (dirvish-preview--image-size preview-window 'height))
           (cache (dirvish-preview--cache-image-path file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("epub-thumbnailer" ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf-preface (file preview-window)
  "Display a pdf preface image for FILE in PREVIEW-WINDOW."
  (when (string= (file-name-extension file) "pdf")
    (let* ((width (dirvish-preview--image-size preview-window))
           (height (dirvish-preview--image-size preview-window 'height))
           (cache (dirvish-preview--cache-image-path file width))
           (cache-jpg (concat cache ".jpg")))
      (if (file-exists-p cache-jpg)
          `(image . ,(create-image cache-jpg nil nil :max-width width :max-height height))
        `(image-cache . ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" ,file ,cache))))))

(dirvish-define-preview pdf-tools (file)
  "Open FILE with `find-file-noselect'."
  (when (string= (file-name-extension file) "pdf")
    `(buffer . ,(find-file-noselect file t nil))))

(dirvish-define-preview archive (file)
  "Display output of corresponding unarchive commands for FILE."
  (cond ((string= (file-name-extension file) "zip")
         `(shell . ("zipinfo" ,file)))
        ((member (file-name-extension file) '("tar" "zst"))
         `(shell . ("tar" "-tvf" ,file)))))

(dirvish-define-preview default (file)
  "Default preview dispatcher for FILE."
  (let ((threshold (or large-file-warning-threshold 10000000))
        (filesize (file-attribute-size (file-attributes file)))
        (enable-local-variables nil))
    (cond ((file-directory-p file) ; in case user did not specify a directory dispatcher
           `(buffer . ,(dired-noselect file)))
          ((> filesize threshold) ; do not preview too large files
           `(info . ,(format "File %s is too big for literal preview." file)))
          (t (dirvish-preview--inhibit-long-line file)))))

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
  (let ((buf (dirvish--get-util-buffer dv 'preview))
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
         (let* ((buf (dirvish--ensure-temp-buffer "img-cache"))
                (path (dirvish-prop :child))
                (name (format "%s-%s-img-cache" path
                              (window-width (dv-preview-window dv)))))
           (unless (get-process name)
             (setq dirvish--cache-pool
                   (delq (assoc name dirvish--cache-pool) dirvish--cache-pool))
             (let ((proc (apply #'start-process name buf cmd args)))
               (process-put proc 'path path)
               (set-process-sentinel proc #'dirvish-preview--img-cache-sentinel))))
         (insert " [Dirvish] Generating image cache..."))
        ('shell
         (let* ((res-buf (dirvish--ensure-temp-buffer "shell-output"))
                (proc (apply #'start-process "dirvish-preview-process" res-buf cmd args)))
           (set-process-sentinel proc 'dirvish-preview--fill-string-sentinel))))
      buf)))

(defun dirvish-preview-update (&optional dv)
  "Update preview content of DV."
  (when-let* ((dv (or dv (dirvish-curr)))
              (window (dv-preview-window dv)))
    (when (window-live-p window)
      (let* ((orig-buffer-list (buffer-list))
             (index (dirvish-prop :child))
             (file (if (file-directory-p index) (file-name-as-directory index) index))
             (buffer (cl-loop for dp-fn in (dv-preview-dispatcher-fns dv)
                              for (type . payload) = (funcall dp-fn file window)
                              thereis (and type (dirvish-preview-dispatch
                                                 type payload dv)))))
        (setq other-window-scroll-buffer buffer)
        (set-window-buffer window buffer)
        (unless (memq buffer orig-buffer-list)
          (push buffer (dv-preview-buffers dv)))
        (with-current-buffer buffer (run-hooks 'dirvish-preview-setup-hook))))))

;;;; Builder

(dirvish-define-attribute hl-line
  :form
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end)))) (overlay-put ov 'face hl-face) ov)))

;; This hack solves 2 issues:
;; 1. Hide " -> " arrow of symlink files as well.
;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
(dirvish-define-attribute symlink-target
  :if (and dired-hide-details-mode
           (default-value 'dired-hide-details-hide-symlink-targets))
  :form
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end))) (overlay-put ov 'invisible t) ov)))

;; Thanks to `doom-modeline'.
(dirvish-define-mode-line bar "Create the bar image."
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (let ((color (or (face-background 'bold nil t) "None"))
           (height (floor (* (if (dirvish-dired-p dv) 0.75 1.2)
                             (default-line-height)))))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" 2 height)
                  (make-string (* 2 height) ?1) "\n")
          'pbm t :foreground color :ascent 'center))))))

(dirvish-define-mode-line path "Current index path."
  (let* ((index (dirvish-prop :child))
         (file-path (or (file-name-directory index) ""))
         (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
         (path-regex (concat (getenv "HOME") "/\\|\\/$"))
         (path-tail (replace-regexp-in-string path-regex "" file-path))
         (file-name (file-name-nondirectory index)))
    (format " %s %s %s"
            (propertize (if path-prefix-home "~" ":"))
            (propertize path-tail 'face 'dired-mark)
            (propertize file-name 'face 'font-lock-constant-face))))

(dirvish-define-mode-line sort "Current sort criteria."
  (let* ((switches (split-string dired-actual-switches))
         (crit (cond (dired-sort-inhibit "DISABLED")
                     ((member "--sort=none" switches) "none")
                     ((member "--sort=time" switches) "time")
                     ((member "--sort=version" switches) "version")
                     ((member "--sort=size" switches) "size")
                     ((member "--sort=extension" switches) "extension")
                     ((member "--sort=width" switches) "width")
                     (t "name")))
         (time (cond ((member "--time=use" switches) "use")
                     ((member "--time=ctime" switches) "ctime")
                     ((member "--time=birth" switches) "birth")
                     (t "mtime")))
         (rev (if (member "--reverse" switches) "↑" "↓")))
    (format " %s|%s|%s "
            (propertize rev 'face 'font-lock-doc-markup-face)
            (propertize crit 'face 'font-lock-type-face)
            (propertize time 'face 'font-lock-doc-face))))

(dirvish-define-mode-line omit "A `dired-omit-mode' indicator."
  (and dired-omit-mode (propertize "[Omit]" 'face 'bold)))

(dirvish-define-mode-line free-space
  "Amount of free space on `default-directory''s file system."
  (format " %s %s "
          (propertize (or (get-free-disk-space default-directory) "")
                      'face 'font-lock-constant-face)
          (propertize "free" 'face 'font-lock-doc-face)))

(dirvish-define-mode-line index "Current file's index and total files count."
  (let ((cur-pos (- (line-number-at-pos (point)) 1))
        (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
      (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold))))

(dirvish-define-mode-line find-dired
  "Return a string showing current `find/fd' command args."
  (if-let ((res-buf-p (dirvish-prop :fd-dir))
           (args (or (bound-and-true-p fd-dired-input-fd-args) find-args)))
      (format " %s [%s] at %s"
              (propertize "FD:" 'face 'bold)
              (propertize args 'face 'font-lock-string-face)
              (propertize default-directory 'face 'dired-header))
    (dirvish-path-ml dv)))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let ((dv (dirvish-curr)))
    (cond ((eobp) (forward-line -1)) ((bobp) (forward-line 1)))
    (dired-move-to-filename)
    (dirvish--render-attributes dv)
    (when-let ((filename (dired-get-filename nil t)))
      (dirvish-prop :child filename)
      (dirvish-debounce layout
        (with-current-buffer (dirvish--get-util-buffer dv 'header)
          (force-mode-line-update))
        (dirvish-preview-update)))))

(defun dirvish-quit-h ()
  "Quit current Dirvish."
  (dirvish-kill (dirvish-prop :dv))
  (switch-to-buffer (dirvish--ensure-temp-buffer)))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function."
  (dired-revert)
  (unless (file-remote-p default-directory)
    (dirvish-preview--clean-cache-images (dired-get-marked-files)))
  (dirvish--hide-dired-header)
  (dirvish-update-body-h))

(defun dirvish-setup (&optional keep-dired)
  "Default config for dirvish parent windows.
If KEEP-DIRED is specified, reuse the old Dired buffer."
  (unless keep-dired
    (setq-local revert-buffer-function #'dirvish-revert)
    (dirvish--hide-dired-header))
  (and (not keep-dired)
       (or (not (dirvish-prop :remote))
           (memq 'vc dirvish-enabled-features-on-remote))
       (dirvish-prop :vc-backend
         (ignore-errors (vc-responsible-backend default-directory))))
  (setq-local face-font-rescale-alist nil)
  (setq-local dired-hide-details-hide-symlink-targets nil)
  (setq-local cursor-type nil)
  (or dirvish--attrs-hash
      (setq-local dirvish--attrs-hash (make-hash-table :test #'equal :size 200)))
  (set-window-fringes nil 1 1)
  (when-let ((child (dirvish-prop :child))) (dired-goto-file child))
  (let* ((dv (dirvish-curr))
         (ml-fmt (dv-mode-line-format dv)))
    (cond ((functionp dirvish-hide-details)
           (funcall dirvish-hide-details dv))
          (dirvish-hide-details
           (let (dired-hide-details-mode-hook)
             (dired-hide-details-mode t))))
    (dirvish--render-attributes dv)
    (push (current-buffer) (dv-dired-buffers dv))
    (dirvish-prop :dv dv)
    (cond ((not (eq (selected-window) (dv-root-window dv)))
           (setq mode-line-format nil))
          (ml-fmt (setq mode-line-format ml-fmt)))
    (setq header-line-format
          (and (dirvish-dired-p dv) (dv-header-line-format dv))))
  (add-hook 'window-buffer-change-functions #'dirvish-reclaim nil t)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil t)
  (add-hook 'quit-window-hook #'dirvish-quit-h nil t)
  (run-hooks 'dirvish-mode-hook))

(defun dirvish-build--parents (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (depth (dv-depth dv))
         (i 0))
    (when (or (file-remote-p current) (dv-no-parents dv))
      (setq depth 0))
    (dirvish-setup (dirvish-prop :dv))
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
              (dirvish-prop :child current)
              (dirvish-setup)
              ;; always hide details in parent windows
              (let (dired-hide-details-mode-hook) (dired-hide-details-mode t)))))))))

(defun dirvish-build--preview (dv)
 "Create a window showing preview for DV."
  (let* ((inhibit-modification-hooks t)
         (buf (dirvish--get-util-buffer dv 'preview))
         (win-alist `((side . right) (window-width . ,dirvish-preview-width)))
         (fringe 30)
         (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
    (set-window-fringes new-window fringe fringe nil t)
    (setf (dv-preview-window dv) new-window)
    (dirvish-cache-images dv)))

(defun dirvish-build--header (dv)
  "Create a window showing header for DV."
  (unless (eq (cdr dirvish-header-height) 0)
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--get-util-buffer dv 'header))
           (win-alist `((side . above)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish--noselect (dir)
  "Return the Dirvish buffer at DIR, do not select it."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (reuse (cl-loop
                 for name in (dirvish-get-all 'name nil t)
                 for dv = (gethash name dirvish--hash)
                 thereis (and (equal (dv-index-dir dv) dir) dv)))
         (dv (or reuse (dirvish-new nil :depth -1))))
    (setf (dv-index-dir dv) dir)
    (ring-insert dirvish--history-ring dir)
    (with-current-buffer (dirvish--buffer-for-dir dv dir)
      (or reuse (dirvish-build dv))
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
      (dirvish-prop :async-p t)
      buf)))

(defun dirvish--buffer-for-dir (dv entry &optional parent)
  "Return the root or PARENT buffer in DV for ENTRY.
If the buffer is not available, create it with `dired-noselect'."
  (let* ((dir-buf (if parent (dv-parent-dir-buf-alist dv) (dv-root-dir-buf-alist dv)))
         (buffer (alist-get entry dir-buf nil nil #'equal))
         (switches (dv-ls-switches dv)))
    (unless buffer
      (let* ((remotep (file-remote-p entry))
             (dir-files (unless (or parent remotep)
                          (directory-files entry t nil t))))
        (if (> (length dir-files) dirvish-async-listing-threshold)
            (setq buffer (dirvish--noselect-async entry switches))
          (setq buffer (dired-noselect entry switches)))
        (with-current-buffer buffer
          (dirvish-mode)
          (setq-local dirvish--props (make-hash-table :size 10))
          (dirvish-prop :remote remotep)
          (dirvish-prop :files dir-files)))
      (push (cons entry buffer)
            (if parent (dv-parent-dir-buf-alist dv) (dv-root-dir-buf-alist dv))))
    buffer))

(defun dirvish--autocache ()
  "Pop and run the cache tasks in `dirvish--cache-pool'."
  (when (and dirvish--cache-pool
             (< (length (process-list))
                (cdr dirvish-auto-cache-threshold)))
    (let (process-connection-type proc)
      (pcase-let* ((`((,path . ,width) . (,cmd . ,args)) (pop dirvish--cache-pool)))
        (when path
          (setq proc (apply #'start-process (format "%s-%s-img-cache" path width)
                            (dirvish--ensure-temp-buffer "img-cache") cmd args))
          (process-put proc 'path path)
          (set-process-sentinel proc #'dirvish-preview--img-cache-sentinel))))))

(defun dirvish-build (dv)
  "Build layout for Dirvish session DV."
  (unless (dirvish-dired-p dv)
    (let ((ignore-window-parameters t)) (delete-other-windows))
    (dirvish-build--preview dv)
    (dirvish-build--header dv))
  (dirvish-build--parents dv))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-cache-images (&optional dv)
  "Cache image/video-thumbnail for index directory in DV.
If called interactively, ignore the directory files count limit
in `dirvish-auto-cache-threshold'."
  (interactive)
  (setq dv (or dv (dirvish-curr)))
  (with-current-buffer (window-buffer (dv-root-window dv))
    (when (or (called-interactively-p 'any)
              (< (length (dirvish-prop :files))
                 (car dirvish-auto-cache-threshold)))
      (cl-loop
       with win = (dv-preview-window dv)
       with width = (window-width win)
       with files = (seq-remove (lambda (s) (string-suffix-p ".gif" s t))
                                (dirvish-prop :files))
       for file in files
       for pl = (cl-loop for fn in dirvish--cache-img-fns
                         for (type . payload) = (funcall fn file win)
                         thereis (and (eq type 'image-cache) payload))
       when pl do (push (cons (cons file width) pl)
                        dirvish--cache-pool)))))

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
            (dirvish-new nil :path parent :depth -1))
        (dirvish-find-file parent)))))

(defun dirvish-toggle-fullscreen ()
  "Toggle fullscreen of current Dirvish."
  (interactive)
  (if-let* ((dv (dirvish-curr))
            (old-depth (dv-depth dv))
            (fs-depth (dv-fullscreen-depth dv))
            (new-depth (if (eq old-depth -1) fs-depth -1))
            (buf (current-buffer)))
      (progn
        (if (dirvish-dired-p dv)
            (with-selected-window (dv-root-window dv)
              (let (quit-window-hook) (quit-window)))
          (set-window-configuration (dv-window-conf dv)))
        (setf (dv-depth dv) new-depth)
        (setf (dv-window-conf dv) (current-window-configuration))
        (with-selected-window (dirvish--create-root-window dv)
          (dirvish-with-no-dedication (switch-to-buffer buf))
          (dirvish-reclaim)
          (dirvish-build dv)
          (dirvish-debounce layout (dirvish-preview-update))))
    (user-error "Dirvish: not in a dirvish buffer")))

(defun dirvish-find-file (&optional file)
  "Find file in dirvish buffer.
FILE can be a file or a directory, if nil then infer entry from
variable `buffer-file-name'.  If IGNORE-HIST is non-nil, do not
update `dirvish--history-ring'."
  (interactive)
  (let* ((entry (or file (dired-get-filename nil t)
                    (user-error "Dirvish: invalid filename")))
         (dv (dirvish-curr)))
    (cond ((string-prefix-p "DIRVISH-FD@" entry)
           (dirvish-with-no-dedication
            (switch-to-buffer (dirvish--buffer-for-dir dv entry))
            (setf (dv-index-dir dv) default-directory)
            (dirvish-build dv)))
          ((file-directory-p entry)
           (let ((entry (file-name-as-directory (expand-file-name entry)))
                 (bname (buffer-file-name (current-buffer)))
                 enable-dir-local-variables)
             (ring-insert dirvish--history-ring entry)
             (setf (dv-index-dir dv) entry)
             (dirvish-with-no-dedication
              (switch-to-buffer (dirvish--buffer-for-dir dv entry))
              (dirvish-prop :child (or bname entry))
              (dirvish-build dv))))
          (t (find-file entry)))))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Override Dired with `dirvish-dired' globally."
  :group 'dirvish :global t
  (if dirvish-override-dired-mode
      (progn
        (dirvish--add-advices '(dired find-dired))
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
  (dirvish-new t :path (or path default-directory) :depth dirvish-depth))

;;;###autoload
(defun dirvish-dired (&optional path other-window)
  "Start a Dirvish session with optional PATH in current window.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to variable `buffer-file-name'.  Execute it
in other window when OTHER-WINDOW is non-nil."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish dired: ")) nil))
  (and other-window (switch-to-buffer-other-window (dirvish--ensure-temp-buffer)))
  (dirvish-new t :path (or path default-directory) :depth -1))

;;;###autoload
(defun dirvish-dwim (&optional path)
  "Run command `dirvish' or `dirvish-dired' for PATH according to window layout.
Enter fullscreen automatically when selected window is the only window."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish: "))))
  (dirvish-new t :path (or path default-directory)
    :depth (if (one-window-p) dirvish-depth -1)))

(provide 'dirvish)
;;; dirvish.el ends here
