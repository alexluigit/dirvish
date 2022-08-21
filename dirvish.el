;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2022 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A minimalistic yet versatile file manager based on Dired.
;; This package gives Dired the following features:
;;
;; - Multiple window layouts
;; - Always available file preview
;; - Isolated sessions
;; - A modern and composable user interface

;;; Code:

(require 'dired)
(require 'so-long)
(require 'ansi-color)
(require 'tramp)
(require 'transient)
(declare-function dirvish-fd "dirvish-fd")
(declare-function dirvish-subtree--prefix-length "dirvish-subtree")

;;;; User Options

(defgroup dirvish nil "A better Dired." :group 'dired)

(defcustom dirvish-attributes '(file-size)
  "File attributes such as `file-size' showing in Dirvish file lines.

You can get all available attributes in `dirvish--available-attrs'.
See `dirvish-define-attribute'."
  :group 'dirvish :type '(repeat (symbol :tag "Dirvish attribute")))

(defcustom dirvish-preview-dispatchers '(image gif video audio epub archive pdf)
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

(defcustom dirvish-preview-disabled-exts '("iso" "bin" "exe" "gpg" "elc" "eln")
  "Do not preview files end with these extensions."
  :group 'dirvish :type '(repeat (string :tag "File name extension")))

(defcustom dirvish-cache-dir
  (expand-file-name "dirvish/" user-emacs-directory)
  "Preview / thumbnail cache directory for dirvish."
  :group 'dirvish :type 'string)

(defcustom dirvish-default-layout '(1 0.11 0.55)
  "Default layout recipe for fullscreen Dirvish sessions.

The value has the form (DEPTH MAX-PARENT-WIDTH PREVIEW-WIDTH).
DEPTH controls the number of windows displaying parent directories.
  It can be 0 if you don't need the parent directories.
MAX-PARENT-WIDTH controls the max width allocated to each parent windows.
PREVIEW-WIDTH controls the width allocated to preview window.
The default value gives us an 1:3:5 (approximately) pane ratio.
Also see `dirvish-layout-recipes' in `dirvish-extras.el'."
  :group 'dirvish :type '(list (integer :tag "number of parent windows")
                               (float :tag "max width of parent windows")
                               (float :tag "width of preview windows")))

(defface dirvish-hl-line
  '((((class color) (background light)) :background "#8eecf4" :extend t)
    (((class color) (background dark)) :background "#004065" :extend t))
  "Face for Dirvish line highlighting."
  :group 'dirvish)

(define-obsolete-variable-alias 'dirvish-mode-line-position 'dirvish-use-mode-line "Aug 5, 2022")
(defcustom dirvish-use-mode-line t
  "Whether to display mode line in dirvish buffers.

The valid value are:
- nil: hide mode line in dirvish sessions
- global: display the mode line across all panes
- t (and others): Display the mode line across directory panes"
  :group 'dirvish :type '(choice (const :tag "Do not show the mode line" nil)
                                 (const :tag "Display the mode line across directory panes" t)
                                 (const :tag "Make the mode line span all panes" global)))

(define-obsolete-variable-alias 'dirvish-header-line-position 'dirvish-use-header-line "Aug 5, 2022")
(defcustom dirvish-use-header-line t
  "Like `dirvish-use-mode-line', but for header line."
  :group 'dirvish :type 'symbol)

(defcustom dirvish-mode-line-height '(25 . 30)
  "Height of Dirvish's mode line.

The value should be a cons cell (H-DIRED . H-DIRVISH), where
H-DIRED and H-DIRVISH represent the height in single window
session and fullscreen session respectively."
  :group 'dirvish
  :type '(choice (interger :tag "Mode line height in all sessions.")
                 (cons (integer :tag "Mode line height in fullscreen sessions.")
                                (integer :tag "Mode line height in single window sessions."))))

(defcustom dirvish-header-line-height '(25 . 35)
  "Like `dirvish-mode-line-height', but for header line."
  :type '(choice interger (cons integer integer)))

(defcustom dirvish-mode-line-format
  '(:left (sort omit symlink) :right (index))
  "Mode line SEGMENTs aligned to left/right respectively.

Set it to nil to use the default `mode-line-format'.  SEGMENT is
a mode line segment defined by `dirvish-define-mode-line' or a
string.  See `dirvish--available-mode-line-segments'."
  :group 'dirvish :type 'plist)

(defcustom dirvish-header-line-format
  '(:left (path) :right ())
  "Like `dirvish-mode-line-format', but for header line ."
  :group 'dirvish :type 'plist)

(defcustom dirvish-hide-details t
  "Whether to hide detailed information on session startup.

The value can be a boolean or a function that takes current
Dirvish session as its argument."
  :group 'dirvish :type '(choice (const :tag "Always hide details" t)
                                 (const :tag "Never hide details" nil)
                                 (function :tag "Custom function")))

(defcustom dirvish-hide-cursor t
  "Whether to hide cursor in dirvish buffers."
  :group 'dirvish :type 'boolean)

(defconst dirvish-image-exts '("webp" "wmf" "pcx" "xif" "wbmp" "vtf" "tap" "s1j" "sjp" "sjpg" "s1g" "sgi" "sgif" "s1n" "spn" "spng" "xyze" "rgbe" "hdr" "b16" "mdi" "apng" "ico" "pgb" "rlc" "mmr" "fst" "fpx" "fbs" "dxf" "dwg" "djv" "uvvg" "uvg" "uvvi" "uvi" "azv" "psd" "tfx" "t38" "svgz" "svg" "pti" "btf" "btif" "ktx2" "ktx" "jxss" "jxsi" "jxsc" "jxs" "jxrs" "jxra" "jxr" "jxl" "jpf" "jpx" "jpgm" "jpm" "jfif" "jhc" "jph" "jpg2" "jp2" "jls" "hsj2" "hej2" "heifs" "heif" "heics" "heic" "fts" "fit" "fits" "emf" "drle" "cgm" "dib" "bmp" "hif" "avif" "avcs" "avci" "exr" "fax" "icon" "ief" "jpg" "macp" "pbm" "pgm" "pict" "png" "pnm" "ppm" "ras" "rgb" "tga" "tif" "tiff" "xbm" "xpm" "xwd" "jpe" "jpeg"))
(defconst dirvish-audio-exts '("ape" "stm" "s3m" "ra" "rm" "ram" "wma" "wax" "m3u" "med" "669" "mtm" "m15" "uni" "ult" "mka" "flac" "axa" "kar" "midi" "mid" "s1m" "smp" "smp3" "rip" "multitrack" "ecelp9600" "ecelp7470" "ecelp4800" "vbk" "pya" "lvp" "plj" "dtshd" "dts" "mlp" "eol" "uvva" "uva" "koz" "xhe" "loas" "sofa" "smv" "qcp" "psid" "sid" "spx" "opus" "ogg" "oga" "mp1" "mpga" "m4a" "mxmf" "mhas" "l16" "lbc" "evw" "enw" "evb" "evc" "dls" "omg" "aa3" "at3" "atx" "aal" "acn" "awb" "amr" "ac3" "ass" "aac" "adts" "726" "abs" "aif" "aifc" "aiff" "au" "mp2" "mp3" "mp2a" "mpa" "mpa2" "mpega" "snd" "vox" "wav"))
(defconst dirvish-video-exts '("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs"))
(defconst dirvish-media-exts (append dirvish-image-exts dirvish-video-exts dirvish-audio-exts '("pdf" "gif")))
(defcustom dirvish-open-with-programs
  `((,dirvish-audio-exts . ("mpv" "%f"))
    (,dirvish-video-exts . ("mpv" "%f")))
  "Association list of mimetype and external program for `find-file'.

Each element is of the form (EXTS . (CMD . ARGS)).  EXTS is a
list of file name extensions.  Once the EXTS is matched with
FILENAME in `find-file', a subprocess according to CMD and its
ARGS is issued to open the file outside of Emacs.  The special
placeholder \"%f\" in the ARGS is replaced by the FILENAME at
runtime.  Set it to nil disables this feature."
  :group 'dirvish
  :type '(alist :key-type ((repeat string) :tag "File mimetype or extensions")
                :value-type ((repeat string) :tag "External command and args")))

(defcustom dirvish-reuse-session t
  "Whether to reuse the hidden sessions.

If non-nil, Dirvish keeps the session's last buffer alive on
exit.  The hidden session can be reused in the future by command
`dirvish' and friends.  If the value is \\='resume, dirvish
exhibits the last entry of the hidden session unless the PATH
argument is specified via prompt."
  :group 'dirvish :type '(choice (const :tag "Do not reuse the session, quit it completely" nil)
                                 (const :tag "Reuse the session and open new path when reusing" t)
                                 (const :tag "Reuse the session and resume its last entry when reusing" resume)))

(defcustom dirvish-whitelist-host-regex nil
  "Regexp of host names that always enable extra features."
  :group 'dirvish :type 'string)

(defcustom dirvish-redisplay-debounce 0.02
  "Input debounce for dirvish UI redisplay.

The UI of dirvish is refreshed only when there has not been new
input for `dirvish-redisplay-debounce' seconds."
  :group 'dirvish :type 'float)

(defvar dirvish-activation-hook nil)
(defvar dirvish-deactivation-hook nil)
(defvar dirvish-after-revert-hook nil)
(defvar dirvish-setup-hook nil)
(defvar dirvish-find-entry-hook nil
  "Hook functions to be executed after `dirvish--find-entry'.
Each function takes DV, ENTRY and BUFFER as its arguments.")

;;;; Internal variables

(defvar dirvish-advice-alist
  '((advice dired                             dirvish-dired-ad               :override)
    (advice dired-jump                        dirvish-dired-jump-ad          :override)
    (advice dired-find-file                   dirvish-find-entry-ad          :override)
    (advice dired-find-alternate-file         dirvish-find-entry-ad          :override)
    (advice dired-mouse-find-file             dirvish-mouse-find-entry-ad    :override)
    (advice dired-find-file-other-window      dirvish-find-file-other-win-ad :override)
    (advice dired-other-window                dirvish-dired-other-window-ad  :override)
    (advice dired-other-tab                   dirvish-dired-other-tab-ad     :override)
    (advice dired-other-frame                 dirvish-dired-other-frame-ad   :override)
    (advice dired-up-directory                dirvish-up-directory-ad        :override)
    (advice dired-dwim-target-next            dirvish-dwim-target-next-ad    :override)
    (advice wdired-change-to-wdired-mode      dirvish-wdired-enter-ad        :after)
    (advice wdired-exit                       dirvish-wdired-exit-ad         :after)
    (advice wdired-finish-edit                dirvish-wdired-exit-ad         :after)
    (advice wdired-abort-changes              dirvish-wdired-exit-ad         :after)
    (advice burly-bookmark-windows            dirvish-burly-save-ad          :before)
    (advice burly-open-bookmark               dirvish-burly-open-ad          :around)
    (advice find-file                         dirvish-find-file-ad           :around)
    (advice recentf-track-opened-file         dirvish-ignore-ad              :around)
    (advice winner-save-old-configurations    dirvish-ignore-ad              :around)
    (advice flycheck-buffer                   dirvish-ignore-ad              :around)
    (advice lsp-deferred                      dirvish-ignore-ad              :around)
    (advice moody-redisplay                   dirvish-ignore-ad              :around)
    (hook   window-selection-change-functions dirvish-focus-change-h)
    (hook   minibuffer-exit-hook              dirvish-deactivate-minibuffer-h)
    (hook   tab-bar-tab-pre-close-functions   dirvish-deactivate-tab-h)
    (hook   delete-frame-functions            dirvish-deactivate-frame-h)))
(defvar dirvish-scopes '(:tab tab-bar--current-tab-index :persp get-current-persp :perspective persp-curr))
(defvar dirvish-libraries
  '((dirvish-widgets  path symlink omit index free-space file-link-number
                      file-user file-group file-time file-size file-modes
                      file-inode-number file-device-number)
    (dirvish-vc       vc-state git-msg vc-diff vc-blame vc-log vc-info)
    (dirvish-media    audio image gif video epub pdf pdf-preface archive)
    (dirvish-collapse collapse)
    (dirvish-icons    all-the-icons vscode-icon)
    (dirvish-subtree  subtree-state)
    (dirvish-yank     yank)))
(defvar dirvish-allow-overlap nil)
(defconst dirvish--dired-free-space
  (or (not (boundp 'dired-free-space)) (eq (bound-and-true-p dired-free-space) 'separate)))
(defconst dirvish--tramp-preview-cmd
  "head -n 1000 %s 2>/dev/null || ls -Alh --group-directories-first %s 2>/dev/null &")
(defconst dirvish--saved-new-tab-choice tab-bar-new-tab-choice)
(defconst dirvish--builtin-attrs '(hl-line symlink-target))
(defconst dirvish--builtin-dps '(tramp disable default))
(defconst dirvish--os-windows-p (memq system-type '(windows-nt ms-dos)))
(defconst dirvish--no-update-preview-cmds
  '(ace-select-window other-window scroll-other-window scroll-other-window-down))
(defvar recentf-list)
(defvar dirvish-redisplay-debounce-timer nil)
(defvar dirvish--selected-window nil)
(defvar dirvish--mode-line-fmt nil)
(defvar dirvish--header-line-fmt nil)
(defvar dirvish--hash (make-hash-table))
(defvar dirvish--available-attrs '())
(defvar dirvish--available-mode-line-segments '())
(defvar dirvish--available-preview-dispatchers '())
(defvar-local dirvish--props '())
(defvar-local dirvish--attrs-hash nil)
(put 'dirvish--props 'permanent-local t)
(put 'dired-subdir-alist 'permanent-local t)

;;;; Helpers

(defmacro dirvish-prop (prop &rest body)
  "Retrive PROP from `dirvish--props'.
Set the PROP with BODY if given."
  (declare (indent defun))
  `(let* ((pair (assq ,prop dirvish--props)) (val (cdr pair)))
     ,(if body `(prog1 (setq val ,@body)
                  (if pair (setcdr (assq ,prop dirvish--props) val)
                    (push (cons ,prop val) dirvish--props)))
        `val)))

(defmacro dirvish-debounce (label &rest body)
  "Debouncing the execution of BODY under LABEL.
The BODY runs only when there has not been new input for DEBOUNCE
seconds.  DEBOUNCE defaults to `dirvish-redisplay-debounce'."
  (declare (indent defun))
  (setq label (or label "redisplay"))
  (let* ((debounce (intern (format "dirvish-%s-debounce" label)))
         (timer (intern (format "dirvish-%s-debounce-timer" label)))
         (fn `(lambda () (ignore-errors ,@body))))
    `(progn
       (and (timerp ,timer) (cancel-timer ,timer))
       (setq ,timer (run-with-idle-timer ,debounce nil ,fn)))))

(defmacro dirvish-with-no-dedication (&rest body)
  "Run BODY after undedicating window."
  (declare (debug (&rest form)))
  `(progn
     (let* ((window (get-buffer-window (current-buffer)))
            (dedicated (window-dedicated-p window)))
       (set-window-dedicated-p window nil)
       ,@body
       (set-window-dedicated-p window dedicated))))

(defun dirvish--hide-dired-header ()
  "Hide the Dired header."
  (when dirvish-use-header-line
    (remove-overlays (point-min) (point-max) 'dirvish-remove-header t)
    (save-excursion
      (goto-char (point-min))
      (let ((o (make-overlay
                (point) (progn (forward-line (if dirvish--dired-free-space 2 1)) (point)))))
        (overlay-put o 'dirvish-remove-header t)
        (overlay-put o 'invisible t)))))

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
         (ignore-window-parameters t)
         (new-window (split-window-no-error nil size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun dirvish--normalize-util-windows (windows)
  "Normalize the size of utility WINDOWS, like header line window."
  (when (and (display-graphic-p) (> emacs-major-version 28))
    (dolist (win windows)
      (let ((window-safe-min-height 0)
            (window-resize-pixelwise t))
        (fit-window-to-buffer win 2 1)))))

(defun dirvish--kill-buffer (buffer)
  "Kill BUFFER when it is a live one."
  (and (buffer-live-p buffer)
       (cl-letf (((symbol-function 'undo-tree-save-history-from-hook) #'ignore)
                 ((symbol-function 'recentf-track-closed-file) #'ignore))
         (kill-buffer buffer))))

(defun dirvish--remove-session (dv)
  "Remove DV from `dirvish--hash' and kill its index buffer."
  (when (or (not dirvish-reuse-session) (eq (dv-type dv) 'split))
    (dirvish--kill-buffer (cdr (dv-index-dir dv)))
    (remhash (dv-name dv) dirvish--hash)))

(defun dirvish--get-project-root (&optional directory)
  "Get project root path of DIRECTORY."
  (when-let* ((pj (project-current nil directory))
              (pj-root (car (with-no-warnings (project-roots pj)))))
    (expand-file-name pj-root)))

(defun dirvish--get-parent-path (path)
  "Get parent directory of PATH."
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun dirvish--append-metadata (metadata completions)
  "Append METADATA for minibuffer COMPLETIONS."
  (let ((entry (if (functionp metadata)
                   `(metadata (annotation-function . ,metadata))
                 `(metadata (category . ,metadata)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

(defun dirvish--window-selected-p (dv)
  "Return t if session DV is selected."
  (eq (dv-root-window dv) dirvish--selected-window))

(defun dirvish--host-in-whitelist-p (&optional vec)
  "Check if the TRAMP connection VEC should be dominated by Dirvish."
  (when-let ((vec (or vec (dirvish-prop :tramp))))
    (or (tramp-local-host-p vec)
        (and dirvish-whitelist-host-regex
             (string-match-p dirvish-whitelist-host-regex (nth 4 vec)))
        (and (tramp-get-method-parameter vec 'tramp-direct-async)
             (tramp-get-connection-property vec "direct-async-process" nil)))))

(defun dirvish--get-scopes ()
  "Return computed scopes according to `dirvish-scopes'."
  (cl-loop for (k v) on dirvish-scopes by 'cddr
           append (list k (and (functionp v) (funcall v)))))

(defun dirvish--format-menu-heading (title &optional note)
  "Format TITLE as a menu heading.
When NOTE is non-nil, append it the next line."
  (let ((no-wb (= (frame-bottom-divider-width) 0)))
    (format "%s%s%s"
            (propertize title 'face `(:inherit dired-mark :overline ,no-wb)
                        'display '((height 1.1)))
            (propertize " " 'face `(:inherit dired-mark :overline ,no-wb)
                        'display '(space :align-to right))
            (propertize (if note (concat "\n" note) "") 'face 'font-lock-doc-face))))

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

(cl-defmacro dirvish-define-attribute (name docstring (&key if width) &rest body)
  "Define a Dirvish attribute NAME.
An attribute contains a pair of predicate/rendering functions
that are being called on `post-command-hook'.  The predicate fn
IF takes current DV as argument and executed once.  When it
evaluates to t, the rendering fn runs BODY for every line with
following arguments:

- `f-beg'   from `dired-move-to-filename'
- `f-end'   from `dired-move-to-end-of-filename'
- `f-str'   from (`buffer-substring' F-BEG F-END)
- `f-wid'   from `(`string-width' F-STR)'
- `f-dir'   from `dired-current-directory'
- `f-name'  from `dired-get-filename'
- `f-attrs' from `file-attributes'
- `f-type'  from `file-directory-p' along with `file-symlink-p'
- `l-beg'   from `line-beginning-position'
- `l-end'   from `line-end-position'
- `remain'  remained space (width) of current line
- `hl-face' a face that is only passed in on current line

DOCSTRING is the docstring for the attribute.  WIDTH designates
the length of the attribute."
  (declare (indent defun) (doc-string 2))
  (let* ((ov (intern (format "dirvish-%s-ov" name)))
         (pred (intern (format "dirvish-attribute-%s-pred" name)))
         (render (intern (format "dirvish-attribute-%s-rd" name)))
         (args '(f-beg f-end f-str f-wid f-dir f-name f-attrs f-type l-beg l-end remain hl-face))
         (pred-body (if (> (length if) 0) if t)))
    `(progn
       (add-to-list
        'dirvish--available-attrs
        (cons ',name '(:doc ,docstring :width ,width :overlay ,ov :if ,pred :fn ,render)))
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

(cl-defmacro dirvish-define-preview (name &optional arglist docstring &rest body)
  "Define a Dirvish preview dispatcher NAME.
A dirvish preview dispatcher is a function consumed by
 `dirvish-preview-dispatch' which takes `file' (filename under
 the cursor) and `preview-window' as ARGLIST.  DOCSTRING and BODY
 is the docstring and body for this function."
  (declare (indent defun) (doc-string 3))
  (let* ((dp-name (intern (format "dirvish-%s-preview-dp" name)))
         (default-arglist '(file ext preview-window dv))
         (ignore-list (cl-set-difference default-arglist arglist))
         (keywords `(:doc ,docstring)))
    (while (keywordp (car body)) (dotimes (_ 2) (push (pop body) keywords)))
    `(progn
       (add-to-list
        'dirvish--available-preview-dispatchers (cons ',name ',keywords))
       (cl-loop
        with doc-head = "All available `dirvish-preview-dispatchers'.
This is a internal variable and should *NOT* be set manually.  To
get rid of the warnings upon session initialization, please
install the dependencies (recommended) or remove corresponding
items from `dirvish-preview-dispatchers'."
        with dp-docs = ""
        with dps = (seq-remove (lambda (i) (memq (car i) dirvish--builtin-dps))
                               dirvish--available-preview-dispatchers)
        for (dp-name . dp-plist) in dps
        do (setq dp-docs (format "%s\n\n`%s': %s" dp-docs dp-name
                                 (plist-get dp-plist :doc)))
        finally do (put 'dirvish--available-preview-dispatchers 'variable-documentation
                        (format "%s%s" doc-head dp-docs)))
       (defun ,dp-name ,default-arglist ,docstring (ignore ,@ignore-list) ,@body))))

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
        with seg-docs = ""
        for (seg-name . doc) in dirvish--available-mode-line-segments
        do (setq seg-docs (format "%s\n\n`%s': %s" seg-docs seg-name doc))
        finally do (put 'dirvish--available-mode-line-segments 'variable-documentation
                        (format "%s%s" doc-head seg-docs)))
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
  (type nil :documentation "is the session type, such as \\='side.")
  (path nil :documentation "is the initial directory.")
  (layout () :documentation "is the working layout.")
  (last-fs-layout dirvish-default-layout :documentation "is the last fullscreen layout.")
  (attributes (purecopy dirvish-attributes) :documentation "is the actual `dirvish-attributes'.")
  (attribute-fns () :documentation "are render functions expanded from ATTRIBUTES.")
  (preview-dispatchers dirvish-preview-dispatchers :documentation "are actual preview dispatchers.")
  (preview-fns () :documentation "are preview functions expanded from PREVIEW-DISPATCHERS.")
  (ls-switches dired-listing-switches :documentation "is the listing switches.")
  (root-window-fn #'frame-selected-window :documentation "is the function to create the ROOT-WINDOW.")
  (root-window nil :documentation "is the main window created by ROOT-WINDOW-FN.")
  (on-file-open #'dirvish-on-file-open :documentation "Function to run before opening a file.")
  (on-winconf-change #'dirvish-winconf-change-h :documentation "Called when window config changes.")
  (scopes () :documentation "are the \"environments\" such as init frame of this session.")
  (preview-buffers () :documentation "holds all file preview buffers in this session.")
  (preview-window nil :documentation "is the window to display preview buffer.")
  (name (cl-gensym) :documentation "is an unique symbol for every session.")
  (window-conf nil :documentation "is the saved window configuration.")
  (index-dir () :documentation "is a (DIR . CORRESPONDING-BUFFER) cons of ROOT-WINDOW.")
  (roots () :documentation "is the list of all INDEX-DIRs.")
  (parents () :documentation "is like ROOT, but for parent windows."))

(defun dirvish--find-reusable-session (type)
  "Return the first matched reusable session with TYPE."
  (cl-loop
   with scopes = (dirvish--get-scopes)
   for dv-name in (dirvish-get-all 'name t t)
   for dv = (gethash dv-name dirvish--hash)
   for (_ . index-buf) = (dv-index-dir dv)
   thereis (and (not (get-buffer-window index-buf))
                (eq type (dv-type dv))
                (equal (cl-subseq (dv-scopes dv) 10) scopes)
                dv)))

(defun dirvish--reuse-session (&optional dir layout type)
  "Reuse some hidden Dirvish session with TYPE and find DIR in it.
Set layout for the session with LAYOUT."
  (when-let ((dv (dirvish--find-reusable-session type)))
    (prog1 dv
      (if (and (not current-prefix-arg) (eq dirvish-reuse-session 'resume))
          (setq dir nil)
        (setq dir (and dir (if (file-directory-p dir) dir
                             (file-name-directory dir)))))
      (dirvish--save-env dv)
      (with-selected-window (dirvish--create-root-window dv)
        (dirvish-with-no-dedication (switch-to-buffer (cdr (dv-index-dir dv))))
        (setf (dv-layout dv) layout)
        (set-frame-parameter nil 'dirvish--curr dv)
        (dirvish-find-entry-ad (or dir (dirvish-prop :root)))))))

(defun dirvish--save-env (dv)
  "Save the environment information of DV."
  (setf (dv-window-conf dv) (current-window-configuration))
  (setf (dv-scopes dv)
        (append `(:dv ,dv :point ,(point) :mini ,(active-minibuffer-window)
                      :frame ,(selected-frame) :bname ,buffer-file-name)
                (dirvish--get-scopes))))

(defmacro dirvish-new (&rest args)
  "Create a new dirvish struct and put it into `dirvish--hash'.
ARGS is a list of keyword arguments followed by an optional BODY.
The keyword arguments set the fields of the dirvish struct."
  (declare (indent defun))
  (let ((keywords))
    (while (keywordp (car args))
      (dotimes (_ 2) (push (pop args) keywords)))
    (setq keywords (reverse keywords))
    `(let ((old (dirvish-prop :dv))
           (new (make-dirvish ,@keywords)))
       (puthash (dv-name new) new dirvish--hash)
       (dirvish--refresh-slots new)
       (dirvish--save-env new)
       (dirvish--create-root-window new)
       (when (and old (not dirvish-allow-overlap))
         (dirvish-kill old))
       (set-frame-parameter nil 'dirvish--curr new)
       (when-let ((path (dv-path new)))
         (dirvish-find-entry-ad
          (if (file-directory-p path) path (file-name-directory path))))
       (run-hooks 'dirvish-activation-hook)
       ,(when args `(save-excursion ,@args)) ; Body form given
       new)))

(defun dirvish-kill (dv)
  "Kill the dirvish instance DV."
  (let ((index (cdr (dv-index-dir dv))))
    (when (dv-layout dv)
      (with-current-buffer index
        (setq header-line-format dirvish--header-line-fmt))
      (set-window-configuration (dv-window-conf dv))
      (goto-char (plist-get (dv-scopes dv) :point))
      (dirvish-with-no-dedication (switch-to-buffer index))
      (dirvish-prop :minimized t))
    (mapc #'dirvish--kill-buffer (remove index (mapcar #'cdr (dv-roots dv))))
    (mapc #'dirvish--kill-buffer (mapcar #'cdr (dv-parents dv)))
    (mapc #'dirvish--kill-buffer (dv-preview-buffers dv))
    (setf (dv-roots dv) (list (dv-index-dir dv)))
    (setf (dv-parents dv) '())
    (dolist (type '(preview header footer))
      (dirvish--kill-buffer (dirvish--util-buffer type dv)))
    (run-hooks 'dirvish-deactivation-hook)
    (setq tab-bar-new-tab-choice dirvish--saved-new-tab-choice)
    (set-frame-parameter nil 'dirvish--curr nil)))

(defun dirvish-on-file-open (dv)
  "Called before opening a file in Dirvish session DV."
  (dirvish-kill dv)
  (dirvish--remove-session dv))

(defun dirvish--create-root-window (dv)
  "Create root window of DV."
  (let ((win (funcall (dv-root-window-fn dv))))
    (setf (dv-root-window dv) win) win))

(defun dirvish--preview-dps-validate (dps)
  "Check if the requirements of dispatchers DPS are met."
  (cl-loop with res = ()
           with fmt = "[Dirvish]: install '%s' executable to preview %s files.
See `dirvish--available-preview-dispatchers' for details."
           for dp in (append '(tramp disable) dps '(default))
           for info = (alist-get dp dirvish--available-preview-dispatchers)
           for requirements = (plist-get info :require)
           for met = t
           do (progn (dolist (pkg requirements)
                       (unless (executable-find pkg)
                         (message fmt pkg dp) (setq met nil)))
                     (when met (push (intern (format "dirvish-%s-preview-dp" dp)) res)))
           finally return (reverse res)))

(defun dirvish--refresh-slots (dv)
  "Update dynamic slot values of DV."
  (cl-loop
   with dv-attrs = (dv-attributes dv)
   with dps = (dv-preview-dispatchers dv)
   with (m . h) = (cons dirvish-mode-line-format dirvish-header-line-format)
   with (ml-l . ml-r) = (cons (plist-get m :left) (plist-get m :right))
   with (hl-l . hl-r) = (cons (plist-get h :left) (plist-get h :right))
   with feat-reqs = (append dps ml-l ml-r hl-l hl-r)
   with attrs = dirvish--builtin-attrs
   for (lib . feat) in dirvish-libraries do
   (let ((m-attr (cl-intersection feat dv-attrs))
         (feat-in-lib (cl-intersection feat feat-reqs)))
     (when (or m-attr feat-in-lib) (require lib))
     (and m-attr (setq attrs (append attrs m-attr))))
   finally do
   (setf dirvish--mode-line-fmt (dirvish--mode-line-fmt-setter ml-l ml-r))
   (setf dirvish--header-line-fmt (dirvish--mode-line-fmt-setter hl-l hl-r t))
   (setf (dv-preview-fns dv) (dirvish--preview-dps-validate dps))
   (setf (dv-attribute-fns dv)
         (cl-loop
          for name in attrs
          for attr = (cdr (assoc name dirvish--available-attrs)) collect
          (cl-destructuring-bind
              (&key overlay if fn width &allow-other-keys)
              attr (list overlay if fn width))))))

(defun dirvish--render-attributes-1 (height width subtrees pos tramp fns)
  "HEIGHT WIDTH SUBTREES POS TRAMP FNS."
  (forward-line (- 0 height))
  (cl-dotimes (_ (* 2 height))
    (when (eobp) (cl-return))
    (let ((f-beg (dired-move-to-filename))
          (f-end (dired-move-to-end-of-filename t))
          (l-beg (line-beginning-position))
          (l-end (line-end-position))
          (width (- width (if subtrees (dirvish-subtree--prefix-length) 0)))
          f-str f-wid f-dir f-name f-attrs f-type hl-face)
      (setq hl-face (and (eq (or f-beg l-beg) pos) 'dirvish-hl-line))
      (when f-beg
        (setq f-str (buffer-substring f-beg f-end))
        (setq f-wid (string-width f-str))
        (setq f-dir (dired-current-directory))
        (setq f-name (expand-file-name f-str f-dir))
        (setq f-attrs (dirvish-attribute-cache f-name :builtin
                        (unless tramp (file-attributes f-name))))
        (setq f-type (dirvish-attribute-cache f-name :type
                       (let ((ch (progn (back-to-indentation) (char-after))))
                         `(,(if (eq ch 100) 'dir 'file) . nil))))
        (unless (get-text-property f-beg 'mouse-face)
          (dired-insert-set-properties l-beg l-end)))
      (dolist (fn (if f-beg fns '(dirvish-attribute-hl-line-rd)))
        (funcall fn f-beg f-end f-str f-wid f-dir f-name
                 f-attrs f-type l-beg l-end width hl-face)))
    (forward-line 1)))

(defun dirvish--render-attributes (dv)
  "Render attributes in Dirvish session DV's body."
  (cl-loop with tramp = (dirvish-prop :tramp)
           with subtrees = (bound-and-true-p dirvish-subtree--overlays)
           with height = (frame-height) ; use `window-height' here breaks `dirvish-narrow'
           with width = (- (window-width) (if (dirvish-prop :gui) 0 2)) with fns = ()
           for (ov pred fn wd) in (dv-attribute-fns dv)
           do (remove-overlays (point-min) (point-max) ov t)
           when (funcall pred dv) do
           (progn (setq width (- width (or (eval wd) 0))) (push fn fns))
           finally do (with-silent-modifications
                        (save-excursion (dirvish--render-attributes-1
                                         height width subtrees (point) tramp fns)))))

;;;; Advices

(defun dirvish-dired-ad (dirname &optional switches)
  "Override `dired' command.
DIRNAME and SWITCHES are same with command `dired'."
  (if (dirvish-curr)
      (dirvish-find-entry-ad dirname)
    (or (dirvish--reuse-session dirname)
        (dirvish-new :path dirname :ls-switches switches))))

(defun dirvish-dired-other-window-ad (dirname &optional switches)
  "Override `dired-other-window' command.
DIRNAME and SWITCHES are same with command `dired'."
  (when-let ((dv (dirvish-curr)))
    (when (dv-layout dv) (dirvish-kill dv)))
  (switch-to-buffer-other-window (dirvish--util-buffer))
  (dirvish-new :type 'split :path dirname :ls-switches switches))

(defun dirvish-dired-other-tab-ad (dirname &optional switches)
  "Override `dired-other-tab' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (switch-to-buffer-other-tab "*scratch*")
  (with-current-buffer "*scratch*" ; why do we need this?
    (dirvish-new :type 'split :path dirname
      :ls-switches switches :layout dirvish-default-layout)))

(defun dirvish-dired-other-frame-ad (dirname &optional switches)
  "Override `dired-other-frame' command.
DIRNAME and SWITCHES are the same args in `dired'."
  (switch-to-buffer-other-frame (dirvish--util-buffer))
  (dirvish-new :path dirname :ls-switches switches :layout dirvish-default-layout))

(defun dirvish-dired-jump-ad (&optional other-window file-name)
  "Override `dired-jump' command.
OTHER-WINDOW and FILE-NAME are the same args in `dired-jump'."
  (let ((file-name (or file-name default-directory)))
    (and other-window (switch-to-buffer-other-window (dirvish--util-buffer)))
    (if (dirvish-curr)
        (dirvish-find-entry-ad file-name)
      (or (dirvish--reuse-session file-name) (dirvish-new :path file-name)))))

(defun dirvish-find-entry-ad (&optional entry)
  "Find file in dirvish buffer.
ENTRY can be a filename or a string with format of
`dirvish-fd-bufname' used to query or create a `fd' result
buffer, it defaults to filename under the cursor when it is nil."
  (let* ((entry (or entry (dired-get-filename)))
         (dv (or (dirvish-curr) (user-error "Not in a dirvish session")))
         (buffer (dirvish--find-entry dv entry)))
    (if buffer
        (dirvish-with-no-dedication (switch-to-buffer buffer))
      (find-file entry))))

(defun dirvish-mouse-find-entry-ad (ev &optional find-file-fn find-dir-fn)
  "Find file via mouse event EV in dirvish buffer.
The optional arguments FIND-FILE-FN and FIND-DIR-FN specify
functions to visit the file and directory, respectively."
  (let ((win (posn-window (event-end ev)))
        (pos (posn-point (event-end ev))) file)
    (unless (windowp win) (error "No file chosen"))
    (select-window win)
    (with-current-buffer (window-buffer win)
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (cond ((not find-file-fn) (dirvish-find-entry-ad file))
          ((file-directory-p file) (funcall find-dir-fn file))
          (t (funcall find-file-fn file)))))

(defun dirvish-up-directory-ad (&optional other-window)
  "Override `dired-up-directory' command.
If OTHER-WINDOW, display the parent directory in other window."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent-path current)))
    (if (string= parent current)
        (user-error "Dirvish: you're in root directory")
      (if other-window
          (progn
            (switch-to-buffer-other-window (dirvish--util-buffer))
            (dirvish-new :path parent))
        (dirvish-find-entry-ad parent))
      (dired-goto-file current))))

(defun dirvish-find-file-other-win-ad (&rest _)
  "Override `dired-find-file-other-window' command."
  (let ((dv (dirvish-curr))
        (file (dired-get-file-for-visit)))
    (if (dv-layout dv)
        (if (file-directory-p file)
            (dired-other-frame file)
          (dirvish-kill (dirvish-prop :dv))
          (switch-to-buffer-other-window (current-buffer))
          (find-file file))
      (if (file-directory-p file)
          (dired-other-window file)
        (other-window 1)
        (find-file file)))))

(defun dirvish-dwim-target-next-ad (&optional all-frames)
  "Replacement for `dired-dwim-target-next'.
If ALL-FRAMES, search target directories in all frames."
  (delete (when (derived-mode-p 'dired-mode) (dired-current-directory))
          (cl-loop for (dir . buf) in (dirvish-get-all 'index-dir all-frames)
                   when (get-buffer-window buf) collect dir)))

(defun dirvish-wdired-enter-ad (&rest _)
  "Advice for `wdired-change-to-wdired-mode'."
  (dired-move-to-end-of-filename t)
  (cond ((boundp 'evil-normal-state-cursor)
         (setq-local evil-normal-state-cursor 'hollow))
        ((boundp 'meow-cursor-type-normal)
         (setq-local cursor-type 'hollow))
        (dirvish-hide-cursor
         (setq-local cursor-type '(bar . 4))))
  (dolist (ov (mapcar #'car (dv-attribute-fns (dirvish-curr))))
    (remove-overlays (point-min) (point-max) ov t))
  (remove-hook 'post-command-hook #'dirvish-update-body-h t))

(defun dirvish-wdired-exit-ad (&rest _)
  "Advice for exiting `wdired-mode'."
  (dirvish--init-dired-buffer (dirvish-curr))
  (revert-buffer))

(defun dirvish-burly-save-ad (&rest _)
  "Advice for `burly-bookmark-windows'."
  (when-let* ((dv (dirvish-curr)) (fullscreen? (dv-layout dv)))
    (user-error "Dirvish: fullscreen sessions can not be saved as bookmark")))

(defun dirvish-burly-open-ad (fn &rest args)
  "Advice for FN `burly-open-bookmark' and its ARGS."
  (when-let* ((dv (dirvish-curr)) (fullscreen? (dv-layout dv)))
    (dirvish-quit))
  ;; one or more `bookmark-jump' calls happens in the same window,
  ;; so we have to allow overlapping sessions here.
  (let ((dirvish-allow-overlap t)) (apply fn args)))

(defun dirvish-find-file-ad (fn filename &optional wildcard)
  "Advice for FN `find-file' and `find-file-other-window'.
FILENAME and WILDCARD are their args."
  (let* ((ext (downcase (or (file-name-extension filename) "")))
         (file (expand-file-name filename))
         (process-connection-type nil)
         (ex (cl-loop
              for (exts . (cmd . args)) in dirvish-open-with-programs
              thereis (and (not (dirvish-prop :tramp))
                           (executable-find cmd)
                           (member ext exts)
                           (append (list cmd) args)))))
    (cond (ex (and (bound-and-true-p recentf-mode)
                   (add-to-list 'recentf-list file))
              (apply #'start-process "" nil "nohup"
                     (cl-substitute file "%f" ex :test 'string=)))
          (t (when-let ((dv (dirvish-prop :dv)))
               (funcall (dv-on-file-open dv) dv))
             (funcall fn file wildcard)))))

(defun dirvish-ignore-ad (&rest orig-fn)
  "Only apply ORIG-FN outside of Dirvish."
  (unless (dirvish-curr) (apply orig-fn)))

;;;; Hooks

(defun dirvish-apply-ansicolor-h (_win pos)
  "Update dirvish ansicolor in preview window from POS."
  (ansi-color-apply-on-region
   pos (save-excursion (goto-char pos) (forward-line (frame-height)) (point))))

(defun dirvish-deactivate-tab-h (tab _only-tab)
  "Deactivate all Dirvish sessions in TAB."
  (dolist (scope (dirvish-get-all 'scopes))
    (when (eq (plist-get scope :tab) (tab-bar--tab-index tab))
      (dirvish-kill (plist-get scope :dv)))))

(defun dirvish-deactivate-frame-h (frame)
  "Deactivate all dvs in FRAME."
  (dolist (scope (dirvish-get-all 'scopes t))
    (when (eq (plist-get scope :frame) frame)
      (dirvish-kill (plist-get scope :dv)))))

(defun dirvish-deactivate-minibuffer-h ()
  "Deactivate Dirvish session in minibuffer."
  (dolist (scope (dirvish-get-all 'scopes t))
    (when (eq (plist-get scope :mini) (active-minibuffer-window))
      (dirvish-kill (plist-get scope :dv)))))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let ((dv (dirvish-curr)))
    (cond ((eobp) (forward-line -1))
          ((bobp) (when dirvish-use-header-line
                    (forward-line (if dirvish--dired-free-space 2 1)))))
    (when dirvish-hide-cursor (dired-move-to-filename))
    (dirvish--render-attributes dv)
    (when-let ((filename (dired-get-filename t t)))
      (dirvish-prop :child (expand-file-name filename))
      (let ((h-buf (dirvish--util-buffer 'header dv t))
            (f-buf (dirvish--util-buffer 'footer dv t))
            (this-cmd this-command))
        (dirvish-debounce nil
          (if (not (dv-layout dv))
              (and (< emacs-major-version 29) (force-mode-line-update))
            (when (and dirvish-use-mode-line (buffer-live-p f-buf))
              (with-current-buffer f-buf (force-mode-line-update)))
            (when (and dirvish-use-header-line (buffer-live-p h-buf))
              (with-current-buffer h-buf (force-mode-line-update)))
            (unless (memq this-cmd dirvish--no-update-preview-cmds)
              (dirvish-preview-update dv))))))))

(defun dirvish-kill-buffer-h ()
  "Remove buffer from session's buffer list."
  (let* ((dv (dirvish-prop :dv))
         (buf (current-buffer))
         (win (get-buffer-window buf)))
    (when (window-live-p win) (set-window-dedicated-p win nil))
    (setf (dv-roots dv) (cl-remove-if (lambda (i) (eq (cdr i) buf)) (dv-roots dv)))
    (unless (dv-roots dv)
      (when-let ((layout (dv-layout dv)))
        (set-window-configuration (dv-window-conf dv))
        (goto-char (plist-get (dv-scopes dv) :point)))
      (remhash (dv-name dv) dirvish--hash)
      (dolist (type '(preview header footer))
        (dirvish--kill-buffer (dirvish--util-buffer type dv))))))

(defun dirvish-focus-change-h (&optional _frame-or-window)
  "Save current session to frame parameters."
  (let ((buf (current-buffer))
        (fr-dv (dirvish-curr))
        (dv (dirvish-prop :dv)))
    (cond ((active-minibuffer-window))
          ((and fr-dv (dv-layout fr-dv)
                (not (get-buffer-window (cdr (dv-index-dir fr-dv))))
                (window-live-p (dv-preview-window fr-dv)))
           (set-window-configuration (dv-window-conf fr-dv))
           (switch-to-buffer buf)
           (setq tab-bar-new-tab-choice dirvish--saved-new-tab-choice)
           (set-frame-parameter nil 'dirvish--curr nil))
          (t (set-frame-parameter nil 'dirvish--curr dv)
             (setq tab-bar-new-tab-choice
                   (if dv "*scratch*" dirvish--saved-new-tab-choice))))
    (setq dirvish--selected-window (selected-window))))

(defun dirvish-winconf-change-h ()
  "Create new session on window split."
  (let ((dv (dirvish-prop :dv))
        (path (dirvish-prop :root))
        (child (dirvish-prop :child)))
    (when (and (not (active-minibuffer-window)) (dirvish-prop :minimized))
      (dirvish--build dv))
    ;; when split new window in single window dirvish
    (when (and (not (dv-layout dv)) (> (length (get-buffer-window-list)) 1))
      (switch-to-buffer "*scratch*")
      (let ((new (dirvish-new :type 'split :path path)))
        (setf (dv-root-window dv) (get-buffer-window (cdr (dv-index-dir dv))))
        (setf (dv-index-dir new) (cons path (current-buffer)))
        (dired-goto-file child)
        (dirvish--render-attributes new)))))

(defun dirvish-on-winbuf-change-h (frame-or-window)
  "Rebuild layout once buffer in FRAME-OR-WINDOW changed."
  (let ((win (frame-selected-window frame-or-window)))
    (with-current-buffer (window-buffer win)
      (when-let ((dv (dirvish-prop :dv))) (dirvish--build dv)))))

;;;; Preview

(defun dirvish--preview-inhibit-long-line (file)
  "Preview FILE unless it contains long lines."
  (cl-letf (((symbol-function 'undo-tree-save-history-from-hook) #'ignore))
    (let* ((vc-follow-symlinks t)
           (buf (find-file-noselect file t)))
      (with-current-buffer buf
        (if (funcall so-long-predicate)
            (let ((fmt "File %s contains very long lines, preview skipped."))
              (kill-buffer buf)
              `(info . ,(format fmt file)))
          `(buffer . ,buf))))))

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
        (dirvish-apply-ansicolor-h nil p-min)))))

(dirvish-define-preview tramp (file _ dv)
  "Preview files with `ls' or `head' for tramp files."
  (when-let ((tramp-info (dirvish-prop :tramp)))
    (if (dirvish--host-in-whitelist-p tramp-info)
        (let ((process-connection-type nil)
              (localname (file-remote-p file 'localname))
              (buf (dirvish--util-buffer 'preview dv)) proc)
          (when-let ((proc (get-buffer-process buf))) (delete-process proc))
          (setq proc (tramp-handle-shell-command
                      (format dirvish--tramp-preview-cmd localname localname) buf))
          (set-process-sentinel
           proc (lambda (proc _sig)
                  (when (memq (process-status proc) '(exit signal))
                    (shell-command-set-point-after-cmd (process-buffer proc)))))
          (set-process-filter
           proc (lambda (proc str) (with-current-buffer (process-buffer proc) (insert str))))
          `(buffer . ,buf))
      '(info . "File preview is not supported in current TRAMP connection"))))

(dirvish-define-preview disable (file ext)
  "Disable preview in some cases."
  (cond
   ((not (file-exists-p file))
    `(info . ,(format "%s does not exist" file)))
   ((not (file-readable-p file))
    `(info . ,(format "%s is not readable" file)))
   ((member ext dirvish-preview-disabled-exts)
    `(info . ,(format "Preview for %s has been disabled" file)))))

(dirvish-define-preview default (file ext)
  "Default preview dispatcher for FILE."
  (let ((threshold (or large-file-warning-threshold 10000000))
        (filesize (file-attribute-size (file-attributes file)))
        (enable-local-variables nil))
    (cond ((file-directory-p file) ; user did not specify a directory dispatcher
           `(buffer . ,(dired-noselect file)))
          ((> filesize threshold)
           `(info . ,(format "File %s is too big for literal preview." file)))
          ((member ext dirvish-media-exts)
           `(info . "Preview disabled for media files"))
          (t (dirvish--preview-inhibit-long-line file)))))

(cl-defgeneric dirvish-preview-dispatch (recipe dv)
  "Return preview buffer generated according to RECIPE in session DV.")

(cl-defmethod dirvish-preview-dispatch ((recipe (head info)) dv)
  "Insert info string from RECIPE into DV's preview buffer."
  (let ((buf (dirvish--util-buffer 'preview dv)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays) (insert (cdr recipe)) buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head buffer)) dv)
  "Use payload of RECIPE as preview buffer of DV directly."
  (let ((p-buf (dirvish--util-buffer 'preview dv)))
    (with-current-buffer p-buf (erase-buffer) (remove-overlays) (cdr recipe))))

(cl-defmethod dirvish-preview-dispatch ((recipe (head shell)) dv)
  "Fill DV's preview buffer with output of sh command from RECIPE."
  (let* ((p-buf (dirvish--util-buffer 'preview dv))
         (r-buf (dirvish--util-buffer "shell-output"))
         (process-connection-type nil)
         (proc (apply #'start-process
                      "dirvish-preview-proc" r-buf (cadr recipe) (cddr recipe))))
    (set-process-sentinel proc 'dirvish--preview-fill-string-sentinel)
    (with-current-buffer p-buf (erase-buffer) (remove-overlays) p-buf)))

(defun dirvish-preview-update (dv)
  "Update preview content of DV."
  (when-let* ((window (dv-preview-window dv))
              ((window-live-p window))
              (index (dirvish-prop :child))
              (orig-bufs (buffer-list))
              (ext (downcase (or (file-name-extension index) "")))
              (buf (cl-loop for fn in (dv-preview-fns dv)
                            for rcp = (funcall fn index ext window dv) thereis
                            (and rcp (dirvish-preview-dispatch rcp dv)))))
    (setq-local other-window-scroll-buffer buf)
    (set-window-buffer window buf)
    (unless (memq buf orig-bufs) (push buf (dv-preview-buffers dv)))))

;;;; Builder

(dirvish-define-attribute hl-line "Highlight current line." ()
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end)))) (overlay-put ov 'face hl-face) ov)))

(dirvish-define-attribute symlink-target "Hide symlink target."
  (:if (and dired-hide-details-mode
            (default-value 'dired-hide-details-hide-symlink-targets)))
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end))) (overlay-put ov 'invisible t) ov)))

(defun dirvish--mode-line-fmt-setter (left right &optional header)
  "Set the `dirvish--mode-line-fmt'.
LEFT and RIGHT are segments aligned to left/right respectively.
If HEADER, set the `dirvish--header-line-fmt' instead."
  (cl-labels ((expand (segments)
                (cl-loop for s in segments collect
                         (if (stringp s) s
                           `(:eval (,(intern (format "dirvish-%s-ml" s)) dv)))))
              (get-font-scale ()
                (let* ((face (if header 'header-line 'mode-line-inactive))
                       (defualt (face-attribute 'default :height))
                       (ml-height (face-attribute face :height)))
                  (cond ((floatp ml-height) ml-height)
                        ((integerp ml-height) (/ (float ml-height) defualt))
                        (t 1)))))
    `((:eval
       (let* ((dv (dirvish-prop :dv))
              (buf (cdr (dv-index-dir dv)))
              (scale ,(get-font-scale))
              (win-width (floor (/ (window-width) scale)))
              (str-l "") (str-r "") (len-r 0))
         (when (buffer-live-p buf)
           (setq str-l (format-mode-line
                        ',(or (expand left) mode-line-format) nil nil buf))
           (setq str-r (format-mode-line ',(expand right) nil nil buf))
           (setq len-r (string-width str-r)))
         (concat
          (dirvish--bar-image (dv-layout dv) ,header)
          (if (< (+ (string-width str-l) len-r) win-width)
              str-l
            (let ((trim (1- (- win-width len-r))))
              (if (>= trim 0)
                  (substring str-l 0 (min trim (1- (length str-l))))
                "")))
          (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(ceiling (* scale (string-width str-r)))))))
          str-r))))))

;; Thanks to `doom-modeline'.
(defun dirvish--bar-image (fullscreenp header)
  "Create a bar image with height of `dirvish-mode-line-height'.
If FULLSCREENP, use the `cdr' of the value as height, otherwise
use `car'.  If HEADER, use `dirvish-header-line-height' instead."
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (let* ((hv (if header dirvish-header-line-height dirvish-mode-line-height))
           (ht (cond ((numberp hv) hv) (fullscreenp (cdr hv)) (t (car hv)))))
      (propertize
       " " 'display
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" 2 ht) (make-string (* 2 ht) ?1) "\n")
          'pbm t :foreground "None" :ascent 'center))))))

(defun dirvish--setup-mode-line (layout)
  "Setup the mode/header line according to LAYOUT."
  (setq mode-line-format
        (unless (or layout (not dirvish-use-mode-line))
          dirvish--mode-line-fmt)
        header-line-format
        (cond ((or layout (not dirvish-use-header-line)) nil)
              (t (or (dirvish-prop :cus-header) dirvish--header-line-fmt)))))

(defun dirvish-revert (&optional _arg _noconfirm)
  "Reread the Dirvish buffer.
Dirvish sets `revert-buffer-function' to this function."
  (dirvish-prop :old-index (dired-get-filename nil t))
  (dired-revert)
  (dirvish--hide-dired-header)
  (setq dirvish--attrs-hash (make-hash-table :test #'equal))
  (dirvish--print-directory
   (dirvish-prop :tramp) (current-buffer) default-directory)
  (run-hooks 'dirvish-after-revert-hook))

(defun dirvish--noselect (dir)
  "Return the Dirvish buffer at DIR, do not select it."
  (when (dirvish-curr) (set-window-dedicated-p (selected-window) nil))
  (let ((dir (file-name-as-directory (expand-file-name dir)))
        (dv (or (and dirvish-allow-overlap (dirvish-new))
                (dirvish--find-reusable-session nil)
                (dirvish-curr)
                (dirvish-new))))
    (dirvish--find-entry dv dir)))

(defun dirvish--init-dired-window (dv window)
  "Initialize the Dired WINDOW for session DV."
  (dirvish--setup-mode-line (dv-layout dv)) ; for layout switching
  (set-window-fringes nil 1 1)
  (when (window-parameter window 'window-side)
    (setq-local window-size-fixed 'width))
  (set-window-dedicated-p
   window (or (dv-layout dv) (window-parameter window 'window-side))))

(defun dirvish--init-dired-buffer (dv)
  "Initialize a Dired buffer for session DV."
  (dirvish-mode)
  (when dirvish-hide-cursor
    (setq-local cursor-type nil)
    (cond ((boundp 'evil-normal-state-cursor)
           (setq-local evil-normal-state-cursor '(bar . 0)))
          ((boundp 'meow-cursor-type-default)
           (setq-local meow-cursor-type-motion nil
                       meow-cursor-type-default nil))))
  (setq dirvish--attrs-hash (make-hash-table :test #'equal))
  (setq-local revert-buffer-function #'dirvish-revert)
  (setq-local dired-hide-details-hide-symlink-targets nil)
  (dirvish--hide-dired-header)
  (dirvish--setup-mode-line (dv-layout dv))
  (cond ((functionp dirvish-hide-details) (funcall dirvish-hide-details dv))
        (dirvish-hide-details (dired-hide-details-mode t)))
  (add-hook 'window-buffer-change-functions #'dirvish-on-winbuf-change-h nil t)
  (add-hook 'window-configuration-change-hook (dv-on-winconf-change dv) nil t)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil t)
  (add-hook 'kill-buffer-hook #'dirvish-kill-buffer-h nil t)
  (run-hooks 'dirvish-mode-hook)
  (set-buffer-modified-p nil))

(defun dirvish--find-entry (dv entry &optional parent idx)
  "Return the root or PARENT buffer in DV for ENTRY.
When IDX, select that file."
  (let ((pairs (if parent (dv-parents dv) (dv-roots dv)))
        (bname (plist-get (dv-scopes dv) :bname)) buffer)
    (cond ((string-prefix-p "🔍" entry)
           (setq buffer (or (alist-get entry pairs nil nil #'equal)
                            (pcase-let ((`(,pattern ,dir ,_)
                                         (split-string (substring entry 1) "📁")))
                              (dirvish-fd dir pattern)))))
          ((file-directory-p entry)
           (setq entry (file-name-as-directory (expand-file-name entry)))
           (setq buffer (alist-get entry pairs nil nil #'equal))
           (unless buffer
             (cl-letf (((symbol-function 'dired-insert-set-properties) #'ignore))
               (setq buffer (dired-noselect entry (dv-ls-switches dv))))
             (with-current-buffer buffer (dirvish--init-dired-buffer dv))
             (push (cons entry buffer) (if parent (dv-parents dv) (dv-roots dv))))
           (with-current-buffer buffer
             (unless parent (dirvish-prop :root entry))
             (dirvish-prop :dv dv)
             (dirvish-prop :tramp
               (and (tramp-tramp-file-p entry) (tramp-dissect-file-name entry)))
             (dirvish-prop :gui (display-graphic-p))
             (dired-goto-file (or idx bname entry))
             (dirvish--render-attributes dv))))
    (prog1 buffer (and buffer (run-hook-with-args
                               'dirvish-find-entry-hook dv entry buffer)))))

(defun dirvish--create-parent-windows (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent-path current))
         (parent-dirs ())
         (depth (or (car (dv-layout dv)) 0))
         (i 0))
    (dirvish--init-dired-window dv (selected-window))
    (while (and (< i depth) (not (string= current parent)))
      (setq i (1+ i))
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent-path current))
      (setq parent (dirvish--get-parent-path parent)))
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
                 (buffer (dirvish--find-entry dv parent t current))
                 (window (display-buffer
                          buffer `(dirvish--display-buffer . ,win-alist))))
            (with-selected-window window
              (dirvish--init-dired-window dv window)
              ;; always hide details in parent windows
              (dired-hide-details-mode t))))))))

(defun dirvish--init-util-buffers (dv)
  "Initialize util buffers for DV."
  (with-current-buffer (dirvish--util-buffer 'preview dv)
    (normal-mode)
    (setq mode-line-format nil)
    (add-hook 'window-scroll-functions #'dirvish-apply-ansicolor-h nil t))
  (with-current-buffer (dirvish--util-buffer 'header dv)
    (dirvish-prop :dv dv)
    (setq cursor-type nil)
    (setq window-size-fixed 'height)
    (setq mode-line-format nil))
  (with-current-buffer (dirvish--util-buffer 'footer dv)
    (dirvish-prop :dv dv)
    (setq cursor-type nil)
    (setq window-size-fixed 'height)
    (setq header-line-format nil)
    (setq mode-line-format dirvish--mode-line-fmt)))

(defun dirvish--print-directory-sentinel (proc _exit)
  "Parse the directory metadata from PROC's output STR."
  (let* ((buf (process-get proc 'dv-buf))
         (vec (process-get proc 'vec))
         (entry (process-get proc 'entry))
         (append (process-get proc 'append))
         (str (with-current-buffer (process-buffer proc)
                (substring-no-properties (buffer-string))))
         (info (if vec (split-string str "\n") (read str))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (unless (or vec append) (setq dirvish--attrs-hash (cdr info)))
        (if vec
            (dolist (file (and (> (length info) 2) (cl-subseq info 2 -1)))
              (cl-destructuring-bind
                  (inode priv lnum user group size date time &rest path)
                  (split-string file)
                (let* ((symlinkp (cl-position "->" path :test #'equal))
                       (f-name (string-join (cl-subseq path 0 symlinkp) " "))
                       (f-mtime (concat date " " time))
                       (f-truename (and symlinkp (string-join (cl-subseq path (1+ symlinkp)) " ")))
                       (f-dirp (string-prefix-p "d" priv))
                       (f-attr-type (or f-truename f-dirp)))
                  (puthash (expand-file-name f-name entry)
                           `(:builtin
                             ,(list f-attr-type lnum user group nil f-mtime nil size priv nil inode)
                             :type ,(cons (if f-dirp 'dir 'file) f-truename))
                           dirvish--attrs-hash))))
          (if append (maphash (lambda (k v) (puthash k v dirvish--attrs-hash)) (cdr info))
            (dirvish-prop :vc-backend (car info))))
        (unless append (run-hooks 'dirvish-setup-hook))
        (unless (derived-mode-p 'wdired-mode) (dirvish-update-body-h)))))
  (delete-process proc)
  (kill-buffer (process-buffer proc)))

(defsubst dirvish--directory-printer (entry)
  "Compose attributes printer for ENTRY."
  `(with-temp-buffer
     (let ((hash (make-hash-table :test #'equal))
           (bk ,(and (featurep 'dirvish-vc)
                     `(ignore-errors (vc-responsible-backend ,entry)))))
       (dolist (file (directory-files ,entry t nil t))
         (let* ((attrs (file-attributes file))
                (state (and bk (vc-state-refresh file bk)))
                (git (and (eq bk 'Git) ; TODO: refactor this
                          (shell-command-to-string
                           (format "git log -1 --pretty=%%s %s"
                                   (shell-quote-argument file)))))
                (tp (nth 0 attrs)))
           (cond
            ((eq t tp) (setq tp '(dir . nil)))
            (tp (setq tp `(,(if (file-directory-p tp) 'dir 'file) . ,tp)))
            (t (setq tp '(file . nil))))
           (puthash file `(:builtin ,attrs :type ,tp
                                    ,@(and state (list :vc-state state))
                                    ,@(and git (list :git-msg git)))
                    hash)))
       (prin1 (cons bk hash) (current-buffer)))
     (buffer-substring-no-properties (point-min) (point-max))))

(defun dirvish--print-directory (vec buffer entry &optional append)
  "Fetch `file-attributes' for files in ENTRY, stored locally in BUFFER.
If VEC, the attributes are retrieved by parsing the output of
`ls'.  If APPEND, append the results to the existing hash table."
  (when (or (not vec) (dirvish--host-in-whitelist-p vec))
    (let* ((process-connection-type nil)
           (outbuf (dirvish--util-buffer (make-temp-name "print-dir-")))
           (switches "-1la --human-readable --time-style=long-iso --inode")
           (msg `(message "%s" ,(dirvish--directory-printer entry)))
           (cmd (if vec (format "ls %s %s" switches (file-local-name entry))
                  (format "%S" msg)))
           (proc (if vec (start-file-process-shell-command
                          (buffer-name outbuf) outbuf cmd)
                   (start-process (buffer-name outbuf) outbuf
                                  "emacs" "-Q" "-batch" "--eval" cmd))))
      (process-put proc 'entry entry)
      (process-put proc 'dv-buf buffer)
      (process-put proc 'vec vec)
      (process-put proc 'append append)
      (set-process-sentinel proc #'dirvish--print-directory-sentinel))))

(defun dirvish--window-split-order ()
  "Compute the window split order."
  (let* ((weights '((nil . 0) (t . 1) (global . 2)))
         (ord
          '((00 preview) (12 footer preview header) (21 header preview footer)
            (20 header preview) (11 preview header footer) (10 preview header)
            (01 preview footer) (02 footer preview) (22 footer header preview)))
         (h-pos (if (dirvish-prop :global-header) 2
                  (alist-get dirvish-use-header-line weights)))
         (m-pos (alist-get dirvish-use-mode-line weights))
         (key (string-to-number (format "%s%s" (or h-pos 1) (or m-pos 1)))))
    (cdr (assq key ord))))

(defun dirvish--build (dv)
  "Build layout for Dirvish session DV."
  (setf (dv-index-dir dv) (cons (dirvish-prop :root) (current-buffer)))
  (dirvish-prop :minimized nil)
  (let* ((layout (dv-layout dv))
         (w-order (and layout (dirvish--window-split-order)))
         (w-args `((preview (side . right) (window-width . ,(nth 2 layout)))
                   (header (side . above) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))
                   (footer (side . below) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))))
         maybe-abnormal)
    (setq tab-bar-new-tab-choice "*scratch*")
    (setq dirvish--selected-window (selected-window))
    (dirvish--init-util-buffers dv)
    (when w-order (let ((ignore-window-parameters t)) (delete-other-windows)))
    (dolist (pane w-order)
      (let* ((inhibit-modification-hooks t)
             (buf (dirvish--util-buffer pane dv))
             (win-alist (alist-get pane w-args))
             (win (display-buffer
                          buf `(dirvish--display-buffer . ,win-alist))))
        (cond ((eq pane 'preview) (setf (dv-preview-window dv) win))
              (t (set-window-dedicated-p win t)
                 (push win maybe-abnormal)))
        (set-window-buffer win buf)))
    (dirvish--create-parent-windows dv)
    (let ((h-fmt (or (dirvish-prop :cus-header) dirvish--header-line-fmt))
          (vec (dirvish-prop :tramp)))
      (with-current-buffer (dirvish--util-buffer 'header dv)
        (setq header-line-format h-fmt))
      (dirvish--normalize-util-windows maybe-abnormal)
      (unless (or (dirvish-prop :fd-arglist) (dirvish-prop :cached))
        (dirvish--print-directory vec (current-buffer) default-directory)
        (dirvish-prop :cached t)))))

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'dirvish-dispatch)
    (define-key map (kbd "q") 'dirvish-quit)
    map)
  "Keymap used in a dirvish buffer.")

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Convert Dired buffer to a Dirvish buffer."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-quit ()
  "Quit current Dirvish session."
  (interactive)
  (let ((dv (dirvish-prop :dv)))
    (dirvish-kill dv)
    (quit-window)
    (dirvish--remove-session dv)))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Let Dirvish take over Dired globally."
  :group 'dirvish :global t
  (if dirvish-override-dired-mode
      (progn
        (pcase-dolist (`(,type ,sym ,fn ,place) dirvish-advice-alist)
          (if (eq type 'hook) (add-hook sym fn) (advice-add sym place fn)))
        (setq find-directory-functions
              (cl-substitute #'dirvish--noselect #'dired-noselect find-directory-functions)))
    (pcase-dolist (`(,type ,sym ,fn) dirvish-advice-alist)
      (if (eq type 'hook) (remove-hook sym fn) (advice-remove sym fn)))
    (setq find-directory-functions
          (cl-substitute #'dired-noselect #'dirvish--noselect find-directory-functions))))

;;;###autoload
(defun dirvish (&optional path)
  "Start a full frame Dirvish session with optional PATH.

If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `default-directory'."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Dirvish: "))))
  (setq path (or path default-directory))
  (let ((dv (dirvish-prop :dv)))
    (if (and dv (dv-layout dv))
        (dirvish-find-entry-ad path)
      (or (dirvish--reuse-session path dirvish-default-layout)
          (dirvish-new :path path :layout dirvish-default-layout)))))

(transient-define-prefix dirvish-dispatch ()
  "Main menu for Dired/Dirvish."
  [:description
   (lambda () (dirvish--format-menu-heading
          "Dirvish main menu"
          "Press ? to see more info for the current menu"))
   "Transient commands"
   ("a" "Quick access"           dirvish-quick-access)
   ("h" "Go to history entries"  dirvish-history-menu)
   ("s" "Sort current buffer"    dirvish-quicksort)
   ("l" "Setup listing switches" dirvish-ls-switches-menu)
   ("f" "Setup fd-find switches" dirvish-fd-switches-menu
    :if (lambda () (dirvish-prop :fd-arglist)))
   ("m" "Manage marks"           dirvish-mark-menu)
   ("e" "Manage emerged groups"  dirvish-emerge-menu)
   ("t" "Manage subtrees"        dirvish-subtree-menu)
   ("r" "Rename files"           dirvish-renaming-menu)
   ("v" "Version control system" dirvish-vc-menu)
   ("y" "Yank marked files"      dirvish-yank-menu)
   ("i" "Get file information"   dirvish-file-info-menu)
   "" "Actions | Essential commands"
   ("/" "Perform fd search"      dirvish-fd)
   ("@" "Find all dirs by fd"    dirvish-fd-jump)
   ("n" "Live narrowing"         dirvish-narrow)
   ("u" "User interface setup"   dirvish-setup-menu)
   ("c" "Dired cheatsheet"       dirvish-dired-cheatsheet)]
  (interactive)
  (if dirvish--props (transient-setup 'dirvish-dispatch)
    (user-error "Not in a Dirvish buffer")))

(provide 'dirvish)
;;; dirvish.el ends here
