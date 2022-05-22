;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2022 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.3.21
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
  :group 'dirvish :type '(repeat (symbol :tag "Dirvish attribute")))

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
  :group 'dirvish :type '(repeat (symbol :tag "Dirvish preview dispatcher")))

(defcustom dirvish-preview-disabled-exts
  '("iso" "bin" "exe" "gpg" "elc" "eln")
  "Do not preview files end with these extensions."
  :group 'dirvish :type '(repeat (string :tag "File name extension")))

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

(defvar dirvish--history-ring nil)
(defcustom dirvish-history-length 50
  "Length of directory visiting history Dirvish will track."
  :group 'dirvish :type 'integer
  :set (lambda (k v) (set k v) (setq dirvish--history-ring (make-ring v))))

(defcustom dirvish-default-layout '(1 0.11 0.55)
  "Default layout recipe for fullscreen Dirvish sessions.
The value has the form (DEPTH MAX-PARENT-WIDTH PREVIEW-WIDTH).
DEPTH controls the number of windows displaying parent directories.
  It can be 0 if you don't need the parent directories.
MAX-PARENT-WIDTH controls the max width allocated to each parent windows.
PREVIEW-WIDTH controls the width allocated to preview window.
The default value gives us an 1:3:5 (approximately) pane ratio."
  :group 'dirvish :type '(list (integer :tag "number of parent windows")
                               (float :tag "max width of parent windows")
                               (float :tag "width of preview windows")))

(defface dirvish-hl-line
  '((((class color) (background light)) :background "#8eecf4" :extend t)
    (((class color) (background dark)) :background "#004065" :extend t))
  "Face for Dirvish line highlighting."
  :group 'dirvish)

(defcustom dirvish-mode-line-position 'regular
  "The position to place the mode line in Dirvish sessions.
The valid values are:
- `regular': place it in root window.
- `parent-panes': span all directory panes.
- `global': span all panes."
  :group 'dirvish :type '(choice (const :tag "Like regular mode line" regular)
                                 (const :tag "Span all directory panes" parent-panes)
                                 (const :tag "Span all panes" global)))

(defcustom dirvish-header-line-text-size '(1.0 . 1.1)
  "Text height in Dirvish's header line.
The value should be a cons cell (H-DIRED . H-DIRVISH), where
H-DIRED and H-DIRVISH represent the text height in single window
session and fullscreen session respectively.  If H-DIRVISH is 0,
don't create the header window.  To make this setting take effect
immediately, you'll need to reevaluate your Dirvish mode-line
segments after setting this value."
  :group 'dirvish
  :type '(cons (float :tag "Header text height in fullscreen sessions.")
               (float :tag "Header text height in single window sessions.")))

(defconst dirvish--hl-scale (cons 0.75 1.5))
(defun dirvish--mode-line-fmt-setter (fmt &optional header)
  "Compose the `mode-line-format' or header-line (if HEADER) from FMT."
  (cl-labels ((expand (part)
                (cl-loop for s in (plist-get fmt part) collect
                         (if (stringp s) s `(:eval (,(intern (format "dirvish-%s-ml" s)) dv)))))
              (gets (&optional lg) (if lg (cdr dirvish--hl-scale) (car dirvish--hl-scale)))
              (geth (&optional lg) (funcall (if lg #'cdr #'car) dirvish-header-line-text-size))
              (font-scale ()
                (let ((scale (- (face-attribute 'default :height) 100)))
                  (if (> scale 0) (* scale -0.0075) 0)))
              (getr (t-size) (if (< t-size 1) (/ (- 1 t-size) t-size) 0)))
    `((:eval
       (let* ((dv (dirvish-curr))
              (fullscreenp (dv-layout dv))
              (buf (alist-get (dv-index-dir dv)
                              (dv-root-dir-buf-alist dv) nil nil #'equal))
              (height ,(if header `(if fullscreenp ,(geth t) ,(geth)) 1))
              (win-width (floor (/ (window-width) height)))
              (raise ,(if header `(if fullscreenp ,(font-scale) ,(getr (geth))) 0))
              (str-left
               (propertize (format-mode-line
                            ',(or (expand :left) mode-line-format) nil nil buf)
                           'display `((height ,height) (raise ,raise))))
              (str-right
               (propertize (format-mode-line ',(or (expand :right)) nil nil buf)
                           'display `((height ,height) (raise ,raise))))
              (str-right-length (string-width str-right))
              (str-length (+ (string-width str-left) str-right-length))
              (filling-spaces
               (propertize
                " " 'display
                `((space :align-to (- (+ right right-fringe right-margin)
                                      ,(ceiling (* height (string-width str-right)))))))))
         (concat
          ,(when header `(format-mode-line
                          '(:eval (dirvish--bar-image (if fullscreenp ,(gets t) ,(gets))))))
          (if (< str-length win-width)
              str-left
            (let ((trim (1- (- win-width str-right-length))))
              (if (>= trim 0) (substring str-left 0 trim) "")))
          filling-spaces str-right))))))

(defcustom dirvish-mode-line-format
  '(:left (sort omit symlink) :right (index))
  "Mode line SEGMENTs aligned to left/right respectively.
Set it to nil to use the default `mode-line-format'.  SEGMENT is
a mode line segment defined by `dirvish-define-mode-line' or a
string.  You can get all available SEGMENTs by evaluating:

\(prog1 (mapc #'require `dirvish-extra-libs')
       (describe-variable 'dirvish--available-mode-line-segments))"
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

(defvar dirvish--subtree-prefix-len 2)
(defcustom dirvish-subtree-line-prefix "  "
  "A string put into each nested subtree.
The prefix is repeated \"depth\" times."
  :type 'string :group 'dirvish
  :set (lambda (k v)
         (set k v)
         (setq dirvish--subtree-prefix-len
               (cond ((bound-and-true-p dired-subtree-line-prefix)
                      (or (ignore-errors (length (bound-and-true-p dired-subtree-line-prefix))) 2))
                     (t (length v))))))

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
    (dired         dired-find-file-other-window    dirvish-find-file-other-win-ad :override)
    (dired         dired-other-window              dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab                 dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame               dirvish-dired-other-frame-ad   :override)
    (dired         dired-up-directory              dirvish-up-directory           :override)
    (dired         +dired/quit-all                 quit-window                    :override)
    (dired         dired-current-directory         dirvish-curr-dir-ad            :around)
    (dired         dired-get-subdir                dirvish-get-subdir-ad          :around)
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
(defvar dirvish-scopes
  '(:tab tab-bar--current-tab-index :frame selected-frame :mini active-minibuffer-window))
(defvar dirvish-hook-alist
  '((emacs window-selection-change-functions dirvish-reclaim)
    (emacs minibuffer-exit-hook              dirvish--deactivate-for-minibuffer)
    (emacs tab-bar-tab-pre-close-functions   dirvish--deactivate-for-tab)
    (emacs delete-frame-functions            dirvish--deactivate-for-frame)))
(defvar dirvish-debug-p nil)
(defvar dirvish-override-dired-mode nil)
(defvar dirvish-extra-libs '(dirvish-extras dirvish-vc dirvish-yank))
(defvar fd-dired-generate-random-buffer)
(defconst dirvish--prefix-spaces 2)
(defconst dirvish--debouncing-delay 0.02)
(defconst dirvish--cache-img-threshold (* 1024 1024 0.4))
(defconst dirvish--dir-tail-regex (concat (getenv "HOME") "/\\|\\/$\\|^\\/"))
(defconst dirvish--preview-img-scale 0.92)
(defconst dirvish--saved-new-tab-choice tab-bar-new-tab-choice)
(defconst dirvish--saved-window-combination-resize window-combination-resize)
(defconst dirvish--builtin-attrs '(hl-line symlink-target))
(defconst dirvish--os-windows-p (memq system-type '(windows-nt ms-dos)))
(defconst dirvish--cache-embedded-video-thumb
  (string-match "prefer embedded image" (shell-command-to-string "ffmpegthumbnailer -h")))
(defconst dirvish--cache-img-fns
  (cl-loop for dp in '(image video epub)
           collect (intern (format "dirvish-%s-preview-dp" dp))))
(defvar dirvish--hash (make-hash-table))
(defvar dirvish--available-attrs '())
(defvar dirvish--available-mode-line-segments '())
(defvar dirvish--cache-pool '())
(defvar-local dirvish--props (make-hash-table :size 10))
(defvar-local dirvish--attrs-width `(,dirvish--prefix-spaces . 0))
(defvar-local dirvish--attrs-hash nil)
(defvar-local dirvish--subtree-overlays nil "Subtree overlays in this buffer.")
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

(defun dirvish--goto-file (filename)
  "Go to line describing FILENAME."
  (goto-char (point-min))
  (let (stop)
    (while (and (not stop)
                (= (forward-line) 0))
      (when (equal filename (dired-get-filename nil t))
        (setq stop t)
        (dired-move-to-filename)))
    stop))

(defun dirvish--get-project-root ()
  "Get root path of current project."
  (when-let ((pj (project-current)))
    (car (with-no-warnings (project-roots pj)))))

(defun dirvish--get-parent (path)
  "Get parent directory of PATH."
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun dirvish--subtree-depth ()
  "Get subtree depth at point."
  (let ((dps (cl-loop for ov in (overlays-at (point)) collect
                      (or (overlay-get ov 'dired-subtree-depth) 0))))
    (or (and dps (apply #'max dps)) 0)))

(defun dirvish--subtree-expanded-p ()
  "70x Faster version of `dired-subtree--is-expanded-p'."
  (save-excursion (< (dirvish--subtree-depth)
                     (progn (forward-line 1) (dirvish--subtree-depth)))))

(defun dirvish--subtree-parent (&optional p)
  "Get the parent subtree overlay at point P."
  (setq p (or p (point)))
  (cl-loop
   with (pov . max) = (cons nil 0)
   for ov in (overlays-at p)
   for depth = (or (overlay-get ov 'dired-subtree-depth) 0) do
   (when (> depth max) (setq pov ov) (setq max depth))
   finally return pov))

(defun dirvish--subtree-readin (dirname)
  "Read in the directory DIRNAME for `dirvish--subtree-insert'."
  (let ((switches (or dired-actual-switches dired-listing-switches)))
    (with-temp-buffer
      (insert-directory dirname switches nil t)
      (delete-char -1)
      (goto-char (point-min))
      (delete-region (point) (progn (forward-line 1) (point)))
      (save-match-data
        (while (re-search-forward "^ *d.* \\.\\.?/?\n" nil t)
          (delete-region (match-beginning 0) (match-end 0))))
      (goto-char (point-min))
      (unless (looking-at-p "  ")
        (let ((indent-tabs-mode nil))
          (indent-rigidly (point-min) (point-max) 2)))
      (buffer-string))))

(defun dirvish--subtree-insert ()
  "Insert subtree under this directory."
  (when (and (save-excursion (beginning-of-line) (looking-at "..[dl]"))
             (not (dirvish--subtree-expanded-p)))
    (let* ((dirname (dired-get-filename nil))
           (listing (dirvish--subtree-readin (file-name-as-directory dirname)))
           (inhibit-read-only t)
           beg end)
      (move-end-of-line 1)
      (save-excursion (insert listing) (setq end (+ (point) 2)))
      (newline)
      (setq beg (point))
      (remove-text-properties (1- beg) beg '(dired-filename))
      (let* ((ov (make-overlay beg end))
             (parent (dirvish--subtree-parent (1- beg)))
             (depth (or (and parent (1+ (overlay-get parent 'dired-subtree-depth))) 1)))
        (overlay-put ov 'line-prefix
                     (apply #'concat (make-list depth dirvish-subtree-line-prefix)))
        (overlay-put ov 'dired-subtree-name dirname)
        (overlay-put ov 'dired-subtree-depth depth)
        (overlay-put ov 'evaporate t)
        (push ov dirvish--subtree-overlays)))))

(defun dirvish--subtree-remove ()
  "Remove subtree at point."
  (when-let* ((ov (dirvish--subtree-parent))
              (beg (overlay-start ov))
              (end (overlay-end ov)))
    (let ((inhibit-read-only t))
      (goto-char beg)
      (dired-previous-line 1)
      (delete-region (overlay-start ov) (overlay-end ov))
      (cl-loop for o in (overlays-in (point-min) (point-max))
               when (and (overlay-get o 'dired-subtree-depth)
                         (>= (overlay-start o) beg)
                         (<= (overlay-end o) end))
               do (progn (setq dirvish--subtree-overlays
                               (delq o dirvish--subtree-overlays))
                         (delete-overlay o))))))

(defun dirvish--subtree-revert ()
  "Put the `dired-subtree-overlays' again after buffer reverting."
  (cl-loop
   with st-alist = ()
   for ov in dirvish--subtree-overlays
   for depth = (overlay-get ov 'dired-subtree-depth)
   for name = (overlay-get ov 'dired-subtree-name) do
   (push (cons depth name) st-alist)
   finally do
   (let ((sorted (sort st-alist (lambda (a b) (< (car a) (car b))))))
     (cl-loop for (_depth . name) in sorted do
              (when (dirvish--goto-file name)
                (dirvish--subtree-insert))))
   (dirvish--goto-file (dirvish-prop :child))))

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

(defun dirvish--should-enable (feature)
  "Return t if FEATURE should be enabled."
  (or (not (dirvish-prop :remote))
      (memq feature dirvish-enabled-features-on-remote)))

;;;; Core

(defun dirvish-curr (&optional frame)
  "Get current Dirvish session in FRAME (defaults to selected)."
  (or (dirvish-prop :dv) (frame-parameter frame 'dirvish--curr)))

(defun dirvish--util-buffer (&optional type dv no-create)
  "Return session DV's utility buffer of TYPE (defaults to `temp').
If NO-CREATE is non-nil, do not create the buffer."
  (let* ((id (if dv (format "-%s*" (dv-name dv)) "*"))
         (name (format " *Dirvish-%s%s" (or type "temp") id)))
    (if no-create (get-buffer name) (get-buffer-create name))))

(defun dirvish--init-util-buffers (dv)
  "Initialize util buffers for DV."
  (with-current-buffer (dirvish--util-buffer 'preview dv)
    (setq-local cursor-type nil)
    (setq-local mode-line-format nil)
    (add-hook 'window-scroll-functions #'dirvish-apply-ansicolor-h nil t))
  (with-current-buffer (dirvish--util-buffer 'header dv)
    (setq-local cursor-type nil)
    (setq-local header-line-format nil)
    (setq-local window-size-fixed 'height)
    (setq-local face-font-rescale-alist nil)
    (setq-local mode-line-format (dv-header-line-format dv)))
  (unless (eq dirvish-mode-line-position 'regular)
    (with-current-buffer (dirvish--util-buffer 'footer dv)
      (setq-local cursor-type nil)
      (setq-local header-line-format nil)
      (setq-local mode-line-format nil)
      (setq-local window-size-fixed 'height)
      (setq-local face-font-rescale-alist nil)
      (face-remap-add-relative
       'default :background (face-attribute 'mode-line :background)))))

(cl-defmacro dirvish-define-attribute (name docstring (&key if left right) &rest body)
  "Define a Dirvish attribute NAME.
An attribute contains a pair of predicate/rendering functions
that are being called on `post-command-hook'.  The predicate fn
IF takes current DV as argument and executed once.  When it
evaluates to t, the rendering fn runs BODY for every line with
following arguments:

- `f-name'  from `dired-get-filename'
- `f-attrs' from `file-attributes'
- `f-type'  from `file-directory-p' ('dir or 'file)
- `f-beg'   from `dired-move-to-filename'
- `f-end'   from `dired-move-to-end-of-filename'
- `l-beg'   from `line-beginning-position'
- `l-end'   from `line-end-position'
- `hl-face' a face that is only passed in on current line

DOCSTRING is the docstring for the attribute.  LEFT and RIGHT are
length of the attribute, align to left and right respectively."
  (declare (indent defun) (doc-string 2))
  (let* ((ov (intern (format "dirvish-%s-ov" name)))
         (pred (intern (format "dirvish-attribute-%s-pred" name)))
         (render (intern (format "dirvish-attribute-%s-rd" name)))
         (args '(f-name f-attrs f-type f-beg f-end l-beg l-end hl-face))
         (pred-body (if (> (length if) 0) if t)))
    `(progn
       (add-to-list
        'dirvish--available-attrs
        (cons ',name '(:doc ,docstring :left ,left :right ,right
                            :overlay ,ov :if ,pred :fn ,render)))
       (cl-loop
        with doc-head = "All available `dirvish-attributes'.
This is a internal variable and should *NOT* be set manually."
        with attr-docs = ""
        with attrs = (seq-remove (lambda (i) (memq (car i) dirvish--builtin-attrs))
                                 dirvish--available-attrs)
        for (a-name . a-plist) in attrs
        do (setq attr-docs (format "%s\n\n`%s': %s" attr-docs a-name
                                   (plist-get a-plist :doc)))
        finally do (put 'dirvish--available-attrs 'variable-documentation
                        (format "%s%s" doc-head attr-docs)))
       (defun ,pred (dv) (ignore dv) ,pred-body)
       (defun ,render ,args
         (ignore ,@args)
         (let ((ov ,@body)) (and ov (overlay-put ov ',ov t)))))))

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
 `dirvish-preview-dispatch' which takes `file' (filename under
 the cursor) and `preview-window' as ARGLIST.  DOCSTRING and BODY
 is the docstring and body for this function."
  (declare (indent defun) (doc-string 3))
  (let* ((dp-name (intern (format "dirvish-%s-preview-dp" name)))
         (default-arglist '(file preview-window))
         (ignore-list (cl-set-difference default-arglist arglist)))
    `(progn (defun ,dp-name ,default-arglist ,docstring (ignore ,@ignore-list) ,@body))))

(cl-defmacro dirvish-define-mode-line (name &optional docstring &rest body)
  "Define a mode line segment NAME with BODY and DOCSTRING."
  (declare (indent defun) (doc-string 2))
  (let ((ml-name (intern (format "dirvish-%s-ml" name))))
    `(progn
       (add-to-list
        'dirvish--available-mode-line-segments (cons ',name ,docstring))
       (cl-loop
        with doc-head = "All available segments for `dirvish-mode/header-line-format'.
This is a internal variable and should *NOT* be set manually."
        with attr-docs = ""
        for (seg-name . doc) in dirvish--available-mode-line-segments
        do (setq attr-docs (format "%s\n\n`%s': %s" attr-docs seg-name doc))
        finally do (put 'dirvish--available-mode-line-segments 'variable-documentation
                        (format "%s%s" doc-head attr-docs)))
       (defun ,ml-name (dv) ,docstring (ignore dv) ,@body))))

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
  (layout () :documentation "Todo.")
  (last-fs-layout dirvish-default-layout :documentation "Todo.")
  (no-parents nil :documentation "disables showing parent dirs, has the highest privilege.")
  (attributes (purecopy dirvish-attributes) :documentation "is the actual `dirvish-attributes'.")
  (attribute-fns () :documentation "are render functions expanded from ATTRIBUTES.")
  (preview-dispatchers (purecopy dirvish-preview-dispatchers)
                       :documentation "are actual `dirvish-preview-dispatchers'.")
  (preview-fns () :documentation "are preview functions expanded from PREVIEW-DISPATCHERS.")
  (ls-switches dired-listing-switches :documentation "is the listing switches.")
  (header-line-format dirvish-header-line-format :documentation "is the actual header line format.")
  (mode-line-format dirvish-mode-line-format :documentation "is the actual mode line format.")
  (root-window-fn (lambda (_dv) (frame-selected-window))
                  :documentation "is the function to create ROOT-WINDOW.")
  (root-window nil :documentation "is the main window created by ROOT-WINDOW-FN.")
  (find-file-window-fn #'selected-window
                       :documentation "determines the target window for `find-file'.")
  (quit-window-fn #'ignore :documentation "is the function being called on `quit-window'.")
  (scopes () :documentation "are the \"environments\" such as init frame of this session.")
  (dired-buffers () :documentation "holds all dired buffers in this instance.")
  (preview-buffers () :documentation "holds all file preview buffers in this session.")
  (preview-window nil :documentation "is the window to display preview buffer.")
  (name (cl-gensym) :documentation "is an unique symbol for every session.")
  (window-conf (current-window-configuration) :documentation "is the saved window configuration.")
  (root-dir-buf-alist () :documentation "is a alist of (INDEX-DIR . CORRESPONDING-BUFFER).")
  (parent-dir-buf-alist () :documentation "is like ROOT-DIR-BUF-ALIST, but for parent windows.")
  (index-dir "" :documentation "is the `default-directory' in ROOT-WINDOW."))

(defun dirvish-reclaim (&optional frame-or-window)
  "Reclaim current Dirvish in FRAME-OR-WINDOW."
  (let ((old-dv (dirvish-curr))
        (new-dv (and (dirvish-prop :dv))))
    (cond ((or (active-minibuffer-window)
               (and old-dv (eq (frame-selected-window)
                               (dv-preview-window old-dv)))))
          ((and old-dv (string-match " ?*F\\(in\\)?d.**" (buffer-name)))
           (when (dv-layout old-dv)
             (setq other-window-scroll-buffer
                   (window-buffer (dv-preview-window old-dv)))))
          (new-dv
           (setq tab-bar-new-tab-choice "*scratch*")
           (setq window-combination-resize nil) ; avoid transient menu mess up the layout
           (when (dv-layout new-dv)
             (setq other-window-scroll-buffer
                   (window-buffer (dv-preview-window new-dv))))
           (setf (dv-root-window new-dv) (frame-selected-window frame-or-window))
           (dirvish--add-advices)
           (set-frame-parameter nil 'dirvish--curr new-dv))
          (t
           (setq tab-bar-new-tab-choice dirvish--saved-new-tab-choice)
           (setq window-combination-resize dirvish--saved-window-combination-resize)
           (setq other-window-scroll-buffer nil)
           (dirvish--remove-advices
            (and dirvish-override-dired-mode '(dired find-dired fd-dired)))
           (set-frame-parameter nil 'dirvish--curr new-dv)))))

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
         (when (and (dv-layout old) (dv-layout new))
           (dirvish-kill new)
           (user-error "Dirvish: using existed session"))
         (dirvish-kill old))
       (setf (dv-scopes new)
             (cl-loop
              with res-plist = `(:dv ,new :point ,(point))
              for (key value) on dirvish-scopes by 'cddr do
              (setq res-plist (append res-plist (list key (funcall value))))
              finally return res-plist))
       (set-frame-parameter nil 'dirvish--curr new)
       (when-let ((path (dv-path new)))
         (dirvish-find-file (expand-file-name (file-name-directory path))))
       (run-hooks 'dirvish-activation-hook)
       ,(when args `(save-excursion ,@args)) ; Body form given
       new)))

(defun dirvish-kill (dv)
  "Kill a dirvish instance DV and remove it from `dirvish--hash'.
DV defaults to current dirvish instance if not given."
  (let ((conf (dv-window-conf dv)))
    (when (and (dv-layout dv) (window-configuration-p conf))
      (set-window-configuration conf)
      (goto-char (plist-get (dv-scopes dv) :point)))) ; same buffer in different window
  (cl-labels ((kill-when-live (b) (and (buffer-live-p b) (kill-buffer b))))
    (mapc #'kill-when-live (dv-dired-buffers dv))
    (mapc #'kill-when-live (dv-preview-buffers dv))
    (dolist (type '(preview header footer))
      (kill-when-live (dirvish--util-buffer type dv))))
  (funcall (dv-quit-window-fn dv) dv)
  (remhash (dv-name dv) dirvish--hash)
  (dirvish-reclaim)
  (run-hooks 'dirvish-deactivation-hook)
  (and dirvish-debug-p (message "leftover: %s" (dirvish-get-all 'name t t))))

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
         (preview-fns
          (cl-loop for dp-name in dp-names collect
                   (intern (format "dirvish-%s-preview-dp" dp-name)))))
    (setf (dv-attribute-fns dv) attrs-alist)
    (setf (dv-preview-fns dv) preview-fns)))

(defun dirvish--render-attributes (dv)
  "Render attributes in Dirvish session DV's body."
  (let* ((enable (dirvish--should-enable 'extras))
         (attrs (dv-attribute-fns dv))
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
          (let ((f-attrs (and enable (dirvish-attribute-cache f-name :builtin
                                       (file-attributes f-name))))
                (f-type (and enable (dirvish-attribute-cache f-name :type
                                      (if (file-directory-p f-name) 'dir 'file))))
                (l-beg (line-beginning-position))
                (l-end (line-end-position))
                (hl-face (and (eq f-beg curr-pos) 'dirvish-hl-line)))
            (let (buffer-read-only)
              (unless (get-text-property f-beg 'mouse-face)
                (dired-insert-set-properties l-beg l-end)))
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

(defun dirvish--deactivate-for-minibuffer ()
  "Deactivate Dirvish session in minibuffer."
  (dolist (scope (dirvish-get-all 'scopes t))
    (when (eq (plist-get scope :mini) (active-minibuffer-window))
      (dirvish-kill (plist-get scope :dv)))))

;;;; Advices

(defun dirvish-subtree-remove-ad (fn &rest _)
  "Advisor for FN `dired-subtree-remove'."
  (dirvish--hide-dired-header (funcall fn))) ; See `dired-hacks' #170

(defun dirvish-dired-ad (dirname &optional switches)
  "Override `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (dirvish-new t :path dirname :ls-switches switches))

(defun dirvish-dired-other-window-ad (dirname &optional switches)
  "Override `dired-other-window' command.
DIRNAME and SWITCHES are same with command `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (when-let ((dv (dirvish-curr)))
    (when (dv-layout dv) (dirvish-kill dv)))
  (switch-to-buffer-other-window (dirvish--util-buffer))
  (dirvish-new t :path dirname :ls-switches switches))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer-other-tab "*scratch*")
  (with-current-buffer "*scratch*" ; why do we need this?
    (dirvish-new t :path dirname :ls-switches switches :layout dirvish-default-layout)))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (let (after-focus-change-function)
    (switch-to-buffer-other-frame (dirvish--util-buffer))
    (dirvish-new t :path dirname :ls-switches switches :layout dirvish-default-layout)))

(defun dirvish-dired-jump-ad (&optional other-window file-name)
  "Override `dired-jump' command.
OTHER-WINDOW and FILE-NAME are the same args in `dired-jump'."
  (interactive
   (list nil (and current-prefix-arg (read-file-name "Dirvish jump to: "))))
  (if (and (dirvish-curr) (not other-window))
      (dirvish-find-file file-name)
    (dirvish-dired (or file-name default-directory) other-window)))

(defun dirvish-find-file-other-win-ad (&rest _)
  "Override `dired-find-file-other-window' command."
  (interactive)
  (let ((dv (dirvish-curr))
        (file (dired-get-file-for-visit)))
    (if (dv-layout dv)
        (if (file-directory-p file)
          (dired-other-frame file)
        (dirvish-kill (dirvish-prop :dv))
        (switch-to-buffer-other-window (find-file file)))
      (if (file-directory-p file)
            (dired-other-window file)
          (other-window 1)
          (find-file file)))))

(defun dirvish-curr-dir-ad (fn &optional localp)
  "Advice for FN `dired-current-directory'.
LOCALP is the arg for `dired-current-directory', which see."
  (if-let ((parent (dirvish--subtree-parent))
           (dir (concat (overlay-get parent 'dired-subtree-name) "/")))
      (if localp (dired-make-relative dir default-directory) dir)
    (funcall fn localp)))

(defun dirvish-get-subdir-ad (&rest fn-args)
  "Advice for FN-ARGS `dired-get-subdir'."
  (unless (dirvish--subtree-parent) (apply fn-args)))

(defun dirvish-find-dired-sentinel-ad (&rest _)
  "Advice function for `find-dired-sentinel'."
  (let ((dv (or (dirvish-curr) (dirvish-new nil)))
        (dirname-str (format "DIRVISH-FD@%s" (dired-current-directory)))
        buffer-read-only)
    (setq-local dirvish--props (make-hash-table :size 10))
    (setf (dv-index-dir dv) dirname-str)
    (dirvish-prop :child (or (dired-get-filename nil t) "."))
    (dirvish-prop :dv dv)
    (dirvish-prop :fd-dir dirname-str)
    (setf (dv-no-parents dv) t)
    (setf (dv-header-line-format dv)
          (dirvish--mode-line-fmt-setter '(:left (find-dired)) t))
    ;; BUG?: `dired-move-to-filename' failed to parse filename when there is only 1 file in buffer
    (delete-matching-lines "find finished at.*\\|^ +$")
    (dirvish--hide-dired-header)
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
  (dolist (ov (mapcar #'car (dv-attribute-fns (dirvish-curr))))
    (remove-overlays (point-min) (point-max) ov t))
  (remove-hook 'post-command-hook #'dirvish-update-body-h t))

(defun dirvish-find-file-ad (args)
  "Advice for `find-file' with its ARGS."
  (when-let ((dv (dirvish-curr)))
    (cond ((> (length (get-buffer-window-list nil nil t)) 1)
           (switch-to-buffer "*scratch*")
           (set-frame-parameter nil 'dirvish--curr nil)
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
  (when packages (dolist (package packages) (require package nil t)))
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

(defun dirvish--preview-image-size (window &optional height)
  "Get corresponding image width or HEIGHT in WINDOW."
  (floor (* dirvish--preview-img-scale
            (funcall (if height #'window-pixel-height #'window-pixel-width) window))))

(defun dirvish--preview-cache-image-path (file size &optional ext no-mkdir)
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

(defun dirvish--preview-insert-image (image dv)
  "Insert IMAGE at preview window of DV."
  (insert " ")
  (add-text-properties 1 2 `(display ,image rear-nonsticky t keymap ,image-map))
  (pcase-let ((`(,i-width . ,i-height) (image-size (image-get-display-property))))
    (let* ((p-window (dv-preview-window dv))
           (w-offset (max (round (/ (- (window-width p-window) i-width) 2)) 0))
           (h-offset (max (round (/ (- (window-height p-window) i-height) 2)) 0)))
      (goto-char 1)
      (insert (make-string h-offset ?\n) (make-string w-offset ?\s)))))

(defun dirvish--preview-inhibit-long-line (file)
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

(defun dirvish--preview-clean-cache-images (fileset)
  "Clean image cache for FILESET."
  (clear-image-cache)
  (let ((win (dv-preview-window (dirvish-curr))) size)
    (when (window-live-p win)
      (setq size (dirvish--preview-image-size win))
      (dolist (file fileset)
        (mapc #'delete-file (file-expand-wildcards
                             (dirvish--preview-cache-image-path file size ".*" t) t))))))

(defun dirvish--preview-fill-string-sentinel (proc _exitcode)
  "A sentinel for dirvish preview process.
When PROC finishes, fill preview buffer with process result."
  (when-let ((dv (dirvish-curr)))
    (with-current-buffer (dirvish--util-buffer 'preview dv)
      (erase-buffer) (remove-overlays)
      (let* ((proc-buf (process-buffer proc))
             (result-str (with-current-buffer proc-buf (buffer-string)))
             (p-min (point-min)))
        (with-current-buffer proc-buf (erase-buffer))
        (insert result-str)
        (ansi-color-apply-on-region
         p-min (progn (goto-char p-min) (forward-line (frame-height)) (point)))))))

(defun dirvish--preview-img-cache-sentinel (proc _exitcode)
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
    (dirvish--preview-inhibit-long-line file)))

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
    (let* ((width (dirvish--preview-image-size preview-window))
           (height (dirvish--preview-image-size preview-window 'height))
           (cache (dirvish--preview-cache-image-path file width ".jpg")))
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
    (let* ((width (dirvish--preview-image-size preview-window))
           (height (dirvish--preview-image-size preview-window 'height))
           (cache (dirvish--preview-cache-image-path file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("ffmpegthumbnailer" "-i" ,file "-o" ,cache "-s"
                         ,(number-to-string width)
                         ,(if dirvish--cache-embedded-video-thumb "-m" "")))))))

(dirvish-define-preview epub (file preview-window)
  "Display a epub thumbnail for FILE in PREVIEW-WINDOW."
  (when (string= (file-name-extension file) "epub")
    (let* ((width (dirvish--preview-image-size preview-window))
           (height (dirvish--preview-image-size preview-window 'height))
           (cache (dirvish--preview-cache-image-path file width ".jpg")))
      (if (file-exists-p cache)
          `(image . ,(create-image cache nil nil :max-width width :max-height height))
        `(image-cache . ("epub-thumbnailer" ,file ,cache ,(number-to-string width)))))))

(dirvish-define-preview pdf-preface (file preview-window)
  "Display a pdf preface image for FILE in PREVIEW-WINDOW."
  (when (string= (file-name-extension file) "pdf")
    (let* ((width (dirvish--preview-image-size preview-window))
           (height (dirvish--preview-image-size preview-window 'height))
           (cache (dirvish--preview-cache-image-path file width))
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
          (t (dirvish--preview-inhibit-long-line file)))))

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
  (let ((buf (dirvish--util-buffer 'preview dv))
        (cmd (car-safe payload))
        (args (cdr-safe payload))
        (path (dirvish-prop :child))
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
        ('image (dirvish--preview-insert-image payload dv))
        ('image-cache
         (let* ((buf (dirvish--util-buffer "img-cache"))
                (name (format "%s-%s-img-cache" path
                              (window-width (dv-preview-window dv)))))
           (unless (get-process name)
             (setq dirvish--cache-pool
                   (delete (assoc name dirvish--cache-pool) dirvish--cache-pool))
             (let ((proc (apply #'start-process name buf cmd args)))
               (process-put proc 'path path)
               (set-process-sentinel proc #'dirvish--preview-img-cache-sentinel))))
         (insert " [Dirvish] Generating image cache..."))
        ('shell
         (let* ((res-buf (dirvish--util-buffer "shell-output"))
                (proc (apply #'start-process "dirvish-preview-process" res-buf cmd args)))
           (set-process-sentinel proc 'dirvish--preview-fill-string-sentinel))))
      buf)))

(defun dirvish-preview-update (&optional dv)
  "Update preview content of DV."
  (when-let* ((dv (or dv (dirvish-curr)))
              (window (dv-preview-window dv))
              (index (dirvish-prop :child)))
    (when (window-live-p window)
      (let* ((orig-buffer-list (buffer-list))
             (file (if (file-directory-p index) (file-name-as-directory index) index))
             (buffer (cl-loop for dp-fn in (dv-preview-fns dv)
                              for (type . payload) = (funcall dp-fn file window)
                              thereis (and type (dirvish-preview-dispatch
                                                 type payload dv)))))
        (setq other-window-scroll-buffer buffer)
        (set-window-buffer window buffer)
        (unless (memq buffer orig-buffer-list)
          (push buffer (dv-preview-buffers dv)))
        (with-current-buffer buffer (run-hooks 'dirvish-preview-setup-hook))))))

;;;; Builder

(dirvish-define-attribute hl-line "Highlight current line." ()
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end)))) (overlay-put ov 'face hl-face) ov)))

;; This hack solves 2 issues:
;; 1. Hide " -> " arrow of symlink files as well.
;; 2. A `dired-subtree' bug (https://github.com/Fuco1/dired-hacks/issues/125).
(dirvish-define-attribute symlink-target "Hide symlink target."
  (:if (and dired-hide-details-mode
            (default-value 'dired-hide-details-hide-symlink-targets)))
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end))) (overlay-put ov 'invisible t) ov)))

;; Thanks to `doom-modeline'.
(defun dirvish--bar-image (scale)
  "Create a bar image to avoid the mode-line wobbling.
The bar image has height of `default-line-height' times SCALE."
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (let ((height (floor (* scale (default-line-height)))))
      (propertize
       " " 'display
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" 2 height)
                  (make-string (* 2 height) ?1) "\n")
          'pbm t :foreground "None" :ascent 'center))))))

(dirvish-define-mode-line path
  "Path of file under the cursor."
  (when-let* ((index (or (dirvish-prop :child) (dired-get-filename nil t)))
              (dirname (file-name-directory index))
              (dir-tail (replace-regexp-in-string dirvish--dir-tail-regex "" dirname))
              (tail (if (equal dir-tail "") "" (concat dir-tail " ")))
              (base (file-name-nondirectory index)))
    (format " %s%s%s "
            (propertize (if (string-prefix-p (getenv "HOME") dirname) "~ " ": ")
                        'face 'dired-header)
            (propertize tail 'face 'dired-mark)
            (propertize base 'face 'dired-header))))

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
    (format " %s %s|%s "
            (propertize rev 'face 'font-lock-doc-markup-face)
            (propertize crit 'face 'font-lock-type-face)
            (propertize time 'face 'font-lock-doc-face))))

(dirvish-define-mode-line omit "A `dired-omit-mode' indicator."
  (and dired-omit-mode (propertize "Omit" 'face 'font-lock-negation-char-face)))

(dirvish-define-mode-line symlink
  "Show the truename of symlink file under the cursor."
  (when-let ((file (or (dirvish-prop :child) (dired-get-filename nil t))))
    (when (file-symlink-p file)
      (format " %s %s "
              (propertize "→" 'face 'font-lock-comment-delimiter-face)
              (propertize (file-truename file) 'face 'dired-symlink)))))

(dirvish-define-mode-line index "Current file's index and total files count."
  (let ((cur-pos (- (line-number-at-pos (point)) 1))
        (fin-pos (number-to-string (- (line-number-at-pos (point-max)) 2))))
      (format " %d / %s " cur-pos (propertize fin-pos 'face 'bold))))

(dirvish-define-mode-line find-dired "Show current `find/fd' command args."
  (if-let ((res-buf-p (dirvish-prop :fd-dir))
           (args (or (bound-and-true-p fd-dired-input-fd-args) find-args)))
      (format " %s [%s] at %s"
              (propertize "FD:" 'face 'dired-header)
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
      (let ((h-buf (dirvish--util-buffer 'header dv t))
            (f-buf (dirvish--util-buffer 'footer dv t)))
        (dirvish-debounce layout
          (when (dv-layout dv)
            (when (buffer-live-p h-buf)
              (with-current-buffer h-buf (force-mode-line-update)))
            (when (buffer-live-p f-buf)
              (with-current-buffer f-buf
                (let ((win (if (eq dirvish-mode-line-position 'global)
                               (get-buffer-window f-buf)
                             (get-buffer-window h-buf))))
                  (erase-buffer)
                  (insert (format-mode-line (dv-mode-line-format dv) nil win f-buf)))))
            (dirvish-preview-update)))))))

(defun dirvish-quit-h ()
  "Quit current Dirvish."
  (dirvish-kill (dirvish-prop :dv))
  (switch-to-buffer (dirvish--util-buffer)))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function."
  (dired-revert)
  (dirvish--subtree-revert)
  (unless (file-remote-p default-directory)
    (dirvish--preview-clean-cache-images (dired-get-marked-files)))
  (dirvish--hide-dired-header)
  (dirvish-update-body-h))

(defun dirvish-setup (&optional keep-dired)
  "Default config for dirvish parent windows.
If KEEP-DIRED is specified, reuse the old Dired buffer."
  (unless keep-dired
    (setq-local revert-buffer-function #'dirvish-revert)
    (dirvish--hide-dired-header))
  (and (not keep-dired)
       (dirvish--should-enable 'vc)
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
    (dirvish--add-advices '(evil))
    (cond ((or (not (dv-layout dv))
               (and (eq (selected-window) (dv-root-window dv))
                    (eq dirvish-mode-line-position 'regular)))
           (setq mode-line-format ml-fmt))
          (t (setq mode-line-format nil)))
    (setq header-line-format
          (and (not (dv-layout dv)) (dv-header-line-format dv))))
  (add-hook 'window-buffer-change-functions #'dirvish-reclaim nil t)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil t)
  (add-hook 'quit-window-hook #'dirvish-quit-h nil t)
  (run-hooks 'dirvish-mode-hook))

(defun dirvish--build-parents (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent current))
         (parent-dirs ())
         (depth (or (car (dv-layout dv)) 0))
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
      (let* ((parent-width (nth 1 (dv-layout dv)))
             (remain (- 1 (nth 2 (dv-layout dv)) parent-width))
             (width (min (/ remain depth) parent-width))
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

(defun dirvish--build-preview (dv)
 "Create a window showing preview for DV."
  (let* ((inhibit-modification-hooks t)
         (buf (dirvish--util-buffer 'preview dv))
         (win-alist `((side . right) (window-width . ,(nth 2 (dv-layout dv)))))
         (fringe 30)
         (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
    (set-window-fringes new-window fringe fringe nil t)
    (setf (dv-preview-window dv) new-window)
    (dirvish-cache-images dv)))

(defun dirvish--build-header (dv)
  "Create a window showing header for DV."
  (unless (eq (cdr dirvish-header-line-text-size) 0)
    (let* ((inhibit-modification-hooks t)
           (buf (dirvish--util-buffer 'header dv))
           (win-alist `((side . above)
                        (window-height . -2)
                        (window-parameters . ((no-other-window . t)))))
           (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
      (set-window-buffer new-window buf))))

(defun dirvish--build-footer (dv)
  "Create a window showing footer for DV."
  (let* ((inhibit-modification-hooks t)
         (buf (dirvish--util-buffer 'footer dv))
         (win-alist `((side . below)
                      (window-height . -1)
                      (window-parameters . ((no-other-window . t)))))
         (new-window (display-buffer buf `(dirvish--display-buffer . ,win-alist))))
    (set-window-buffer new-window buf)))

(defun dirvish--noselect (dir)
  "Return the Dirvish buffer at DIR, do not select it."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (dv (dirvish-new nil)))
    (setf (dv-index-dir dv) dir)
    (ring-insert dirvish--history-ring dir)
    (with-current-buffer (dirvish--buffer-for-dir dv dir)
      (dirvish-build dv)
      (current-buffer))))

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
        (cl-letf (((symbol-function 'dired-insert-set-properties) #'ignore))
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
      (pcase-let* ((`(,procname . (,path ,_width ,cmd ,args))
                    (pop dirvish--cache-pool)))
        (when path
          (setq proc (apply #'start-process procname
                            (dirvish--util-buffer "img-cache") cmd args))
          (process-put proc 'path path)
          (set-process-sentinel proc #'dirvish--preview-img-cache-sentinel))))))

(defun dirvish-build (dv)
  "Build layout for Dirvish session DV."
  (dirvish--init-util-buffers dv)
  (when (dv-layout dv)
    (let ((ignore-window-parameters t)) (delete-other-windows))
    (and (eq dirvish-mode-line-position 'global) (dirvish--build-footer dv))
    (dirvish--build-preview dv)
    (dirvish--build-header dv)
    (and (eq dirvish-mode-line-position 'parent-panes) (dirvish--build-footer dv)))
  (dirvish--build-parents dv))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-toggle-subtree ()
  "Insert subtree at point or remove it if it was not present."
  (interactive)
  (if (dirvish--subtree-expanded-p)
      (progn (dired-next-line 1) (dirvish--subtree-remove))
    (save-excursion (dirvish--subtree-insert))))

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
       for (cmd . args) = (cl-loop
                           for fn in dirvish--cache-img-fns
                           for (type . payload) = (funcall fn file win)
                           thereis (and (eq type 'image-cache) payload))
       when cmd do (push (cons (format "%s-%s-img-cache" file width)
                               (list file width cmd args))
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
            (switch-to-buffer-other-window (dirvish--util-buffer))
            (dirvish-new nil :path parent))
        (dirvish-find-file parent)))))

(defun dirvish-toggle-fullscreen ()
  "Toggle fullscreen of current Dirvish."
  (interactive)
  (let* ((dv (dirvish-curr))
         (old-layout (dv-layout dv))
         (new-layout (unless old-layout (dv-last-fs-layout dv)))
         (buf (current-buffer)))
    (if old-layout
        (set-window-configuration (dv-window-conf dv))
      (with-selected-window (dv-root-window dv)
        (let (quit-window-hook) (quit-window))))
    (setf (dv-layout dv) new-layout)
    (setf (dv-window-conf dv) (current-window-configuration))
    (with-selected-window (dirvish--create-root-window dv)
      (dirvish-with-no-dedication (switch-to-buffer buf))
      (dirvish-reclaim)
      (dirvish-build dv)
      (dirvish-debounce layout (dirvish-preview-update)))))

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
            (setf (dv-index-dir dv) entry)
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
  (dirvish-new t :path (or path default-directory) :layout dirvish-default-layout))

;;;###autoload
(defun dirvish-dired (&optional path other-window)
  "Start a Dirvish session with optional PATH in current window.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to variable `buffer-file-name'.  Execute it
in other window when OTHER-WINDOW is non-nil."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish dired: ")) nil))
  (and other-window (switch-to-buffer-other-window (dirvish--util-buffer)))
  (dirvish-new t :path (or path default-directory)))

;;;###autoload
(defun dirvish-dwim (&optional path)
  "Run command `dirvish' or `dirvish-dired' for PATH according to window layout.
Enter fullscreen automatically when selected window is the only window."
  (interactive (list (and current-prefix-arg (read-file-name "Dirvish: "))))
  (dirvish-new t :path (or path default-directory)
    :layout (and (one-window-p) dirvish-default-layout)))

(provide 'dirvish)
;;; dirvish.el ends here
