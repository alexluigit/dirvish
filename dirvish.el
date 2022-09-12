;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2022 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
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
(require 'transient)
(declare-function ansi-color-apply-on-region "ansi-color")
(declare-function dirvish-fd--find "dirvish-fd")
(declare-function dirvish-subtree--prefix-length "dirvish-subtree")
(declare-function dirvish-tramp--noselect "dirvish-tramp")
(declare-function dirvish-tramp--preview-handler "dirvish-tramp")

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
  '((t :inherit highlight :extend t))
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

(defcustom dirvish-mode-line-height 30
  "Height of Dirvish's mode line.
The value should be a cons cell (H-DIRED . H-DIRVISH), where
H-DIRED and H-DIRVISH represent the height in single window
session and fullscreen session respectively.  If this value is a
integer INT, it is seen as a shorthand for (INT . INT)."
  :group 'dirvish
  :type '(choice (interger :tag "Mode line height in all sessions.")
                 (cons (integer :tag "Mode line height in fullscreen sessions.")
                                (integer :tag "Mode line height in single window sessions."))))

(defcustom dirvish-header-line-height 30
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

(defcustom dirvish-redisplay-debounce 0.02
  "Input debounce for dirvish UI redisplay.

The UI of dirvish is refreshed only when there has not been new
input for `dirvish-redisplay-debounce' seconds."
  :group 'dirvish :type 'float)

(defvar dirvish-after-revert-hook nil)
(defvar dirvish-setup-hook nil)
(defvar dirvish-find-entry-hook nil)

;;;; Internal variables

(defvar dirvish-scopes '(:frame selected-frame :tab tab-bar--current-tab-index
                                :persp get-current-persp :perspective persp-curr))
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
(defvar dirvish-reset-keywords '(:free-space))
(defconst dirvish--dired-free-space
  (or (not (boundp 'dired-free-space)) (eq (bound-and-true-p dired-free-space) 'separate)))
(defconst dirvish--preview-variables ; Copied from `consult.el'
  '((inhibit-message . t) (non-essential . t) (delay-mode-hooks . t)
    (enable-dir-local-variables . nil) (enable-local-variables . :safe)))
(defconst dirvish--builtin-attrs '(hl-line symlink-target))
(defconst dirvish--builtin-dps '(tramp disable default))
(defconst dirvish--os-windows-p (memq system-type '(windows-nt ms-dos)))
(defconst dirvish--no-update-preview-cmds
  '(ace-select-window other-window scroll-other-window scroll-other-window-down))
(defvar dirvish-redisplay-debounce-timer nil)
(defvar dirvish--selected-window nil)
(defvar dirvish--mode-line-fmt nil)
(defvar dirvish--header-line-fmt nil)
(defvar dirvish--session-hash (make-hash-table))
(defvar dirvish--parent-hash (make-hash-table :test #'equal))
(defvar dirvish--this nil)
(defvar dirvish--available-attrs '())
(defvar dirvish--available-mode-line-segments '())
(defvar dirvish--available-preview-dispatchers '())
(defvar-local dirvish--props '())
(defvar-local dirvish--attrs-hash nil)
(put 'dirvish--props 'permanent-local t)
(put 'dired-subdir-alist 'permanent-local t)
(put 'wdired--old-marks 'permanent-local t)

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

(defmacro dirvish-save-dedication (&rest body)
  "Run BODY with window undedicated."
  (declare (debug (&rest form)))
  `(progn
     (let* ((window (get-buffer-window (current-buffer)))
            (dedicated (window-dedicated-p window)))
       (set-window-dedicated-p window nil)
       ,@body
       (set-window-dedicated-p window dedicated))))

(defun dirvish--hide-dired-header ()
  "Hide the Dired header."
  (remove-overlays (point-min) (point) 'dired-header t)
  (save-excursion
    (goto-char (point-min))
    (cond ((or (not (looking-at-p dired-subdir-regexp))
               (cdr dired-subdir-alist)))
          (dirvish-use-header-line
           (let* ((ofs (if dirvish--dired-free-space 2 1))
                  (o (make-overlay (goto-char (point-min))
                                   (progn (forward-line ofs) (point)))))
             (overlay-put o 'dired-header t)
             (overlay-put o 'invisible t))))))

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

(defun dirvish--kill-buffer (buffer &optional visible)
  "Kill BUFFER unless VISIBLE."
  (and (buffer-live-p buffer)
       (cl-letf (((symbol-function 'undo-tree-save-history-from-hook) #'ignore)
                 ((symbol-function 'recentf-track-closed-file) #'ignore))
         (let (kill-buffer-query-functions)
           (when (or (not visible) (not (get-buffer-window buffer)))
             (kill-buffer buffer))))))

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
  (eq (if (dv-layout dv) (dv-root-window dv) (frame-selected-window))
      dirvish--selected-window))

(defun dirvish--scopes ()
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

(defsubst dirvish-curr ()
  "Get selected Dirvish session."
  (gethash (dirvish-prop :dv) dirvish--session-hash))

(defun dirvish--util-buffer (type &optional dv no-create inhibit-hiding)
  "Return session DV's utility buffer of TYPE (defaults to `temp').
If NO-CREATE is non-nil, do not create the buffer.
If INHIBIT-HIDING is non-nil, do not hide the buffer."
  (let* ((id (if dv (format "-%s*" (dv-name dv)) "*"))
         (name (format "%s*Dirvish-%s%s" (if inhibit-hiding "" " ") type id)))
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
  `(let* ((md5 (intern (secure-hash 'md5 ,file)))
          (hash (gethash md5 dirvish--attrs-hash))
          (cached (plist-get hash ,attribute))
          (attr (or cached ,@body)))
     (unless cached
       (puthash md5 (append hash (list ,attribute attr)) dirvish--attrs-hash))
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
  "Gather slot value SLOT of all Dirvish in `dirvish--session-hash'.
If ALL-FRAME is non-nil, collect for all frames.
If FLATTEN is non-nil, collect them as a flattened list."
  (cl-loop
   with dv-slot = (intern (format "dv-%s" slot))
   with h-vals = (hash-table-values dirvish--session-hash)
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
  (scopes () :documentation "are the \"environments\" such as init frame of this session.")
  (preview-buffers () :documentation "holds all file preview buffers in this session.")
  (preview-window nil :documentation "is the window to display preview buffer.")
  (name (cl-gensym) :documentation "is an unique symbol for every session.")
  (winconf nil :documentation "is the saved window configuration.")
  (index-dir () :documentation "is a (DIR . CORRESPONDING-BUFFER) cons of ROOT-WINDOW.")
  (roots () :documentation "is the list of all INDEX-DIRs."))

(defun dirvish--find-reusable (&optional type)
  "Return the first matched reusable session with TYPE."
  (cl-loop
   with scopes = (dirvish--scopes)
   for dv in (hash-table-values dirvish--session-hash)
   when (and (eq type (dv-type dv)) (equal (dv-scopes dv) scopes)) collect dv))

(defun dirvish--reuse-session (&optional dir layout type)
  "Reuse some hidden Dirvish session with TYPE and find DIR in it.
Set layout for the session with LAYOUT."
  (when-let ((dv (car (dirvish--find-reusable type))))
    (prog1 dv
      (if (and (not current-prefix-arg) (eq dirvish-reuse-session 'resume))
          (setq dir nil)
        (setq dir (and dir (if (file-directory-p dir) dir
                             (file-name-directory dir)))))
      (with-selected-window (dirvish--create-root-window dv)
        (dirvish-save-dedication (switch-to-buffer (cdr (dv-index-dir dv))))
        (setf (dv-layout dv) layout)
        (setq dirvish--this dv)
        (dirvish-find-entry-a (or dir (dirvish-prop :root)))))))

(defun dirvish-new (&rest args)
  "Create and save a new dirvish struct to `dirvish--session-hash'.
ARGS is a list of keyword arguments for `dirvish' struct."
  (let (slots new)
    (while (keywordp (car args)) (dotimes (_ 2) (push (pop args) slots)))
    (setq new (apply #'make-dirvish (reverse slots)) dirvish--this new)
    (puthash (dv-name new) new dirvish--session-hash)
    (dirvish--refresh-slots new)
    (dirvish--create-root-window new)
    (when-let ((path (dv-path new))) (dirvish-find-entry-a path)) new))

(defun dirvish-kill (dv)
  "Kill the dirvish instance DV."
  (let ((index (cdr (dv-index-dir dv))))
    (when (dv-layout dv)
      (with-current-buffer index
        (setq header-line-format dirvish--header-line-fmt))
      (when-let ((wconf (dv-winconf dv))) (set-window-configuration wconf))
      (dirvish-save-dedication (switch-to-buffer index)))
    (dolist (b (mapcar #'cdr (dv-roots dv))) (dirvish--kill-buffer b t))
    (mapc #'dirvish--kill-buffer (dv-preview-buffers dv))
    (setf (dv-roots dv) (cl-loop for (d . b) in (dv-roots dv) when
                                 (get-buffer-window b) collect (cons d b)))
    (cl-loop for b in (buffer-list) for bn = (buffer-name b) when
             (string-match-p (format " ?\\*Dirvish-.*-%s\\*" (dv-name dv)) bn)
             do (dirvish--kill-buffer b))
    (setq dirvish--parent-hash (make-hash-table :test #'equal))
    (cond ((> (length (dirvish--find-reusable (dv-type dv))) 1)
           (mapc (pcase-lambda (`(,_ . ,b)) (kill-buffer b)) (dv-roots dv)))
          (dirvish-reuse-session (setf (dv-winconf dv) nil))
          (t (mapc (pcase-lambda (`(,_ . ,b)) (kill-buffer b)) (dv-roots dv))))
    (setq dirvish--this nil)))

(defun dirvish-on-file-open (dv)
  "Called before opening a file in Dirvish session DV."
  (dirvish-kill dv))

(defun dirvish--create-root-window (dv)
  "Create root window of DV."
  (let ((w (funcall (dv-root-window-fn dv)))) (setf (dv-root-window dv) w) w))

(defun dirvish--preview-dps-validate (dps)
  "Check if the requirements of dispatchers DPS are met."
  (cl-loop with res = (prog1 '() (require 'ansi-color))
           with fmt = "[Dirvish]: install '%s' executable to preview %s files.
See `dirvish--available-preview-dispatchers' for details."
           with dp-fmt = "dirvish-%s-preview-dp"
           for dp in (append '(tramp disable) dps '(default))
           for info = (alist-get dp dirvish--available-preview-dispatchers)
           for requirements = (plist-get info :require)
           for met = t
           do (progn (dolist (pkg requirements)
                       (unless (executable-find pkg)
                         (message fmt pkg dp) (setq met nil)))
                     (when met (push (intern (format dp-fmt dp)) res)))
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

(defun dirvish--render-attrs-1 (height width subtrees pos remote fns)
  "HEIGHT WIDTH SUBTREES POS REMOTE FNS."
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
        (setq f-name (concat f-dir f-str))
        (setq f-attrs (dirvish-attribute-cache f-name :builtin
                        (unless remote (file-attributes f-name))))
        (setq f-type (dirvish-attribute-cache f-name :type
                       (let ((ch (progn (back-to-indentation) (char-after))))
                         `(,(if (eq ch 100) 'dir 'file) . nil))))
        (unless (get-text-property f-beg 'mouse-face)
          (dired-insert-set-properties l-beg l-end)))
      (dolist (fn (if f-beg fns '(dirvish-attribute-hl-line-rd)))
        (funcall fn f-beg f-end f-str f-wid f-dir f-name
                 f-attrs f-type l-beg l-end width hl-face)))
    (forward-line 1)))

(defun dirvish--render-attrs (&optional dv)
  "Render attributes in DV's dirvish buffer."
  (cl-loop with dv = (or dv (dirvish-curr)) with fns = ()
           with remote = (dirvish-prop :remote)
           with st = (bound-and-true-p dirvish-subtree--overlays)
           with height = (frame-height) ; use `window-height' here breaks `dirvish-narrow'
           with width = (- (window-width) (if (dirvish-prop :gui) 1 2))
           for (ov pred fn wd) in (dv-attribute-fns dv)
           do (remove-overlays (point-min) (point-max) ov t)
           when (funcall pred dv) do
           (progn (setq width (- width (or (eval wd) 0))) (push fn fns))
           finally do (with-silent-modifications
                        (save-excursion (dirvish--render-attrs-1
                                         height width st (point) remote fns)))))

;;;; Advices

(defun dirvish-find-entry-a (&optional entry)
  "Find ENTRY in current dirvish session.
ENTRY can be a filename or a string with format of
`dirvish-fd-bufname' used to query or create a `fd' result
buffer, it defaults to filename under the cursor when it is nil."
  (let* ((entry (or entry (dired-get-filename)))
         (buffer (cond ((string-prefix-p "🔍" entry) (dirvish-fd--find entry))
                       ((file-directory-p entry) (dired-noselect entry)))))
    (if buffer (dirvish-save-dedication (switch-to-buffer buffer))
      (let* ((ext (downcase (or (file-name-extension entry) "")))
             (file (expand-file-name entry))
             (process-connection-type nil)
             (ex (cl-loop
                  for (exts . (cmd . args)) in dirvish-open-with-programs
                  thereis (and (not (dirvish-prop :remote))
                               (executable-find cmd)
                               (member ext exts)
                               (append (list cmd) args)))))
        (if ex (apply #'start-process "" nil "nohup"
                      (cl-substitute file "%f" ex :test 'string=))
          (when-let ((dv (dirvish-curr))) (funcall (dv-on-file-open dv) dv))
          (find-file file))))))

(defun dirvish-dwim-target-next-a (&optional all-frames)
  "Replacement for `dired-dwim-target-next'.
If ALL-FRAMES, search target directories in all frames."
  (delete (when (derived-mode-p 'dired-mode) (dired-current-directory))
          (cl-loop for (dir . buf) in (dirvish-get-all 'index-dir all-frames)
                   when (get-buffer-window buf) collect dir)))

(defun dirvish-insert-subdir-a (dirname &rest _)
  "Setup newly inserted subdir DIRNAME for this Dirvish buffer."
  (dirvish--hide-dired-header)
  (dirvish-data-for-dir dirname (current-buffer) t))

(defun dirvish-wdired-enter-a (&rest _)
  "Advice for `wdired-change-to-wdired-mode'."
  (dired-move-to-end-of-filename t)
  (setq-local cursor-type 'hollow)
  (when (boundp 'evil-normal-state-cursor)
    (setq-local evil-normal-state-cursor 'hollow))
  (dolist (ov (mapcar #'car (dv-attribute-fns (dirvish-curr))))
    (remove-overlays (point-min) (point-max) ov t))
  (remove-hook 'window-configuration-change-hook #'dirvish-winconf-change-h t)
  (remove-hook 'post-command-hook #'dirvish-update-body-h t))

(defun dirvish-wdired-exit-a (&rest _)
  "Advice for exiting `wdired-mode'."
  (dirvish--init-dired-buffer (dirvish-curr))
  (revert-buffer))

;;;; Hooks

(defun dirvish-apply-ansicolor-h (_win pos)
  "Update dirvish ansicolor in preview window from POS."
  (ansi-color-apply-on-region
   (goto-char pos) (progn (forward-line (frame-height)) (point))))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let ((dv (dirvish-curr)))
    (cond ((eobp) (forward-line -1))
          ((cdr dired-subdir-alist))
          ((bobp) (when dirvish-use-header-line
                    (forward-line (if dirvish--dired-free-space 2 1)))))
    (when dirvish-hide-cursor (dired-move-to-filename))
    (dirvish--render-attrs dv)
    (when-let ((filename (dired-get-filename nil t)))
      (dirvish-prop :index filename)
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
  (when-let ((dv (dirvish-curr)) (buf (current-buffer)))
    (let ((win (get-buffer-window buf)))
      (when (window-live-p win) (set-window-dedicated-p win nil)))
    (setf (dv-roots dv) (cl-remove-if (lambda (i) (eq (cdr i) buf)) (dv-roots dv)))
    (unless (dv-roots dv)
      (when-let ((layout (dv-layout dv))
                 (wconf (dv-winconf dv))
                 ((eq buf (window-buffer (selected-window)))))
        (set-window-configuration wconf))
      (remhash (dv-name dv) dirvish--session-hash)
      (cl-loop for b in (buffer-list) for bn = (buffer-name b) when
               (string-match-p (format " ?\\*Dirvish-.*-%s\\*" (dv-name dv)) bn)
               do (dirvish--kill-buffer b))
      (setq dirvish--this nil))))

(defun dirvish-selection-change-h (&optional _frame-or-window)
  "Save current session to frame parameters."
  (let* ((w (frame-selected-window)) (b (window-buffer w)) (dv (dirvish-curr)))
    (cond ((and dv (minibufferp (window-buffer dirvish--selected-window)))
           (with-selected-window (dirvish--create-root-window dv)
             (dirvish-save-dedication (switch-to-buffer b))
             (dirvish--build dv)))
          ((active-minibuffer-window))
          ((and dirvish--this (dv-layout dirvish--this)
                (not (get-buffer-window (cdr (dv-index-dir dirvish--this)) t))
                (window-live-p (dv-preview-window dirvish--this)))
           (set-window-configuration (dv-winconf dirvish--this))
           (switch-to-buffer b)
           (setq dirvish--this nil))
          (t (setq dirvish--this dv)))
    (setq dirvish--selected-window w)))

(defun dirvish-winconf-change-h ()
  "Restore hidden sessions on buffer switching."
  (let ((dv (dirvish-curr)))
    (setf (dv-root-window dv) (get-buffer-window (cdr (dv-index-dir dv))))
    (dirvish-update-body-h)))

(defun dirvish-winbuf-change-h (frame-or-window)
  "Rebuild layout once buffer in FRAME-OR-WINDOW changed."
  (let ((win (frame-selected-window frame-or-window)))
    (with-current-buffer (window-buffer win)
      (when-let ((dv (dirvish-curr))) (dirvish--build dv)))))

;;;; Preview

(dirvish-define-preview tramp (file _ dv)
  "Preview files with `ls' or `head' for tramp files."
  (when-let ((vec (dirvish-prop :tramp)))
    (dirvish-tramp--preview-handler dv file vec)))

(dirvish-define-preview disable (file ext)
  "Disable preview in some cases."
  (cond
   ((not (file-exists-p file))
    `(info . ,(format "%s does not exist" file)))
   ((not (file-readable-p file))
    `(info . ,(format "%s is not readable" file)))
   ((member ext dirvish-preview-disabled-exts)
    `(info . ,(format "Preview for %s has been disabled" file)))))

(defun dirvish--find-file-temporarily (name)
  "Open file NAME temporarily for preview."
  (unless (fboundp 'recentf-track-opened-file) (require 'recentf))
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore)
            ((symbol-function 'undo-tree-save-history-from-hook) #'ignore)
            ((symbol-function 'flycheck-mode-on-safe) #'ignore))
    (let* ((vc-follow-symlinks t)
           (vars (mapcar (pcase-lambda (`(,k . ,v))
                           (list k v (default-value k) (symbol-value k)))
                         dirvish--preview-variables))
           (buf (unwind-protect (progn (pcase-dolist (`(,k ,v . ,_) vars)
                                         (set-default k v) (set k v))
                                       (find-file-noselect name 'nowarn))
                  (pcase-dolist (`(,k ,_ ,d ,v) vars)
                    (set-default k d) (set k v)))))
      (cond ((ignore-errors (buffer-local-value 'so-long-detected-p buf))
             (kill-buffer buf)
             `(info . ,(format "File `%s' with long lines not previewed" name)))
            (t `(buffer . ,buf))))))

(dirvish-define-preview default (file ext)
  "Default preview dispatcher for FILE."
  (when-let ((attrs (ignore-errors (file-attributes file)))
             (size (file-attribute-size attrs)))
    (cond ((file-directory-p file) ; default directory previewer
           (let* ((script `(with-current-buffer (dired-noselect ,file "-AlGh")
                             (buffer-string)))
                  (cmd (format "%S" `(message "\n%s" ,script))))
             `(dired . ("emacs" "-Q" "-batch" "--eval" ,cmd))))
          ((> size (or large-file-warning-threshold 10000000))
           `(info . ,(format "File %s is too big for literal preview." file)))
          ((member ext dirvish-media-exts)
           `(info . "Preview disabled for media files"))
          (t (dirvish--find-file-temporarily file)))))

(cl-defgeneric dirvish-preview-dispatch (recipe dv)
  "Return preview buffer generated according to RECIPE in session DV.")

(cl-defmethod dirvish-preview-dispatch ((recipe (head info)) dv)
  "Insert info string from RECIPE into DV's preview buffer."
  (let ((buf (dirvish--util-buffer 'preview dv nil t)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays) (font-lock-mode -1)
      (insert (cdr recipe)) buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head buffer)) dv)
  "Use payload of RECIPE as preview buffer of DV directly."
  (let ((p-buf (dirvish--util-buffer 'preview dv nil t)))
    (with-current-buffer p-buf (erase-buffer) (remove-overlays) (cdr recipe))))

(defun dirvish-shell-preview-proc-s (proc _exitcode)
  "A sentinel for dirvish preview process.
When PROC finishes, fill preview buffer with process result."
  (when-let ((dv (or (dirvish-curr) dirvish--this)))
    (with-current-buffer (dirvish--util-buffer 'preview dv nil t)
      (erase-buffer) (remove-overlays)
      (insert (with-current-buffer (process-buffer proc) (buffer-string)))
      (pcase (process-get proc 'cmd-info)
        ('shell (font-lock-mode -1) (dirvish-apply-ansicolor-h nil (point-min)))
        ('dired
         (setq-local dired-subdir-alist
                     (list (cons (car (dv-index-dir dv)) (point-min-marker))))
         (setq-local font-lock-defaults
                     '(dired-font-lock-keywords t nil nil beginning-of-line))
         (font-lock-mode 1)
         (when (fboundp 'diredfl-mode) (diredfl-mode))))))
  (kill-buffer (process-buffer proc)))

(defun dirvish--run-shell-for-preview (dv recipe)
  "Dispatch shell cmd with RECIPE for session DV."
  (when-let ((proc (get-buffer-process (get-buffer " *Dirvish-temp*"))))
    (delete-process proc))
  (let ((buf (dirvish--util-buffer 'preview dv nil t))
        (proc (make-process :name "sh-out" :connection-type nil
                            :buffer " *Dirvish-temp*" :command (cdr recipe)
                            :sentinel 'dirvish-shell-preview-proc-s)))
    (process-put proc 'cmd-info (car recipe))
    (with-current-buffer buf (erase-buffer) (remove-overlays) buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head shell)) dv)
  "Fill DV's preview buffer with output of sh command from RECIPE."
  (dirvish--run-shell-for-preview dv recipe))

(cl-defmethod dirvish-preview-dispatch ((recipe (head dired)) dv)
  "Fill DV's preview buffer with output of sh command from RECIPE."
  (dirvish--run-shell-for-preview dv recipe))

(defun dirvish-preview-update (dv)
  "Update preview content of DV."
  (when-let* ((window (dv-preview-window dv))
              ((window-live-p window))
              (index (dirvish-prop :index))
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
  (:if (or (eq major-mode 'dirvish-parent-mode)
           (and dired-hide-details-mode
                (default-value 'dired-hide-details-hide-symlink-targets))))
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
       (let* ((dv (dirvish-curr))
              (buf (and (dv-layout dv) (cdr (dv-index-dir dv))))
              (scale ,(get-font-scale))
              (win-width (floor (/ (window-width) scale)))
              (str-l (format-mode-line
                      ',(or (expand left) mode-line-format) nil nil buf))
              (str-r (format-mode-line ',(expand right) nil nil buf))
              (len-r (string-width str-r)))
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

(defun dirvish--hide-cursor ()
  "Hide cursor in dirvish buffer."
  (when dirvish-hide-cursor
    (setq-local cursor-type nil)
    (cond ((boundp 'evil-normal-state-cursor)
           (setq-local evil-normal-state-cursor '(bar . 0)))
          ((boundp 'meow-cursor-type-default)
           (setq-local meow-cursor-type-motion nil
                       meow-cursor-type-default nil)))))

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
  (dolist (keyword dirvish-reset-keywords) (dirvish-prop keyword nil))
  (dired-revert)
  (dirvish--hide-dired-header)
  (setq dirvish--attrs-hash (make-hash-table))
  (dirvish-data-for-dir default-directory (current-buffer) t)
  (run-hooks 'dirvish-after-revert-hook))

(defun dirvish--init-dired-window (dv window)
  "Initialize the Dired WINDOW for session DV."
  (dirvish--setup-mode-line (dv-layout dv)) ; for layout switching
  (set-window-fringes nil 1 1)
  (let ((side (window-parameter window 'window-side)))
    (when side (setq-local window-size-fixed 'width))
    (set-window-dedicated-p window (or (dv-layout dv) side))))

(defun dirvish--init-dired-buffer (dv)
  "Initialize a Dired buffer for session DV."
  (dirvish-mode)
  (dirvish--hide-cursor)
  (setq dirvish--attrs-hash (make-hash-table))
  (setq-local revert-buffer-function #'dirvish-revert)
  (setq-local tab-bar-new-tab-choice "*scratch*")
  (setq-local dired-hide-details-hide-symlink-targets nil)
  (setq-local dired-kill-when-opening-new-dired-buffer nil)
  (dirvish--hide-dired-header)
  (dirvish--setup-mode-line (dv-layout dv))
  (cond ((functionp dirvish-hide-details) (funcall dirvish-hide-details dv))
        (dirvish-hide-details (dired-hide-details-mode t)))
  (add-hook 'window-buffer-change-functions #'dirvish-winbuf-change-h nil t)
  (add-hook 'window-configuration-change-hook #'dirvish-winconf-change-h nil t)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil t)
  (add-hook 'kill-buffer-hook #'dirvish-kill-buffer-h nil t)
  (run-hooks 'dirvish-mode-hook)
  (set-buffer-modified-p nil))

(defun dirvish-dired-noselect-a (fn dir &optional flags)
  "Return buffer for DIR with FLAGS, FN is `dired-noselect'."
  (let* ((key (file-name-as-directory (expand-file-name dir)))
         (win (or (minibuffer-selected-window) (frame-selected-window)))
         (this dirvish--this)
         (dv (cond ((memq this-command '(dired-other-tab dired-other-frame))
                    (dirvish-new :layout dirvish-default-layout))
                   (t (or this (car (dirvish--find-reusable)) (dirvish-new)))))
         (cmds '(dired-other-tab dired-other-frame dirvish dirvish-dwim))
         (bname buffer-file-name)
         (remote (file-remote-p dir))
         (flags (or flags (dv-ls-switches dv)))
         (buffer (alist-get key (dv-roots dv) nil nil #'equal)))
    (set-window-dedicated-p win nil)
    (unless (or (memq this-command cmds) this) (setf (dv-layout dv) nil))
    (unless buffer
      (cl-letf (((symbol-function 'dired-insert-set-properties) #'ignore))
        (if (not remote) (setq buffer (apply fn (list dir flags)))
          (require 'dirvish-tramp)
          (setq buffer (dirvish-tramp--noselect fn dir flags remote))))
      (with-current-buffer buffer (dirvish--init-dired-buffer dv))
      (push (cons key buffer) (dv-roots dv)))
    (with-current-buffer buffer
      (dirvish-prop :dv (dv-name dv))
      (dirvish-prop :gui (display-graphic-p))
      (dirvish-prop :remote remote)
      (dirvish-prop :root key)
      (dired-goto-file (or bname key))
      (setf (dv-index-dir dv) (cons key buffer))
      (run-hook-with-args 'dirvish-find-entry-hook key buffer)
      buffer)))

(cl-defgeneric dirvish-readin-dir (dir &optional flags)
  "Readin the directory DIR with optional FLAGS as a string."
  (let ((switches (or flags dired-actual-switches dired-listing-switches)))
    (with-temp-buffer
      (insert-directory (file-name-as-directory dir) switches nil t)
      (delete-char -1)
      (delete-region (goto-char (point-min)) (progn (forward-line 1) (point)))
      (unless (looking-at-p "  ")
        (let ((indent-tabs-mode nil))
          (indent-rigidly (point-min) (point-max) 2)))
      (buffer-string))))

(defun dirvish--create-parent-buffer (dv dir index level)
  "Create parent buffer at DIR in DV selecting file INDEX.
LEVEL is the depth of current window."
  (let ((index (directory-file-name index))
        (buf (dirvish--util-buffer (format "parent-%s" level) dv nil t))
        (str (or (gethash dir dirvish--parent-hash) (dirvish-readin-dir dir))))
    (with-current-buffer buf
      (dirvish-parent-mode)
      (dirvish-prop :dv (dv-name dv))
      (dirvish-prop :remote (file-remote-p dir))
      (puthash dir str dirvish--parent-hash)
      (erase-buffer)
      (setq mode-line-format nil header-line-format nil)
      (save-excursion (insert str "\n"))
      (setq-local dired-subdir-alist (list (cons dir (point-min-marker))))
      (setq-local font-lock-defaults
                  '(dired-font-lock-keywords t nil nil beginning-of-line))
      (font-lock-mode 1)
      (dired-goto-file-1 (file-name-nondirectory index) index (point-max))
      (dirvish--hide-cursor)
      (setq dirvish--attrs-hash (make-hash-table))
      (when (fboundp 'diredfl-mode) (diredfl-mode))
      (add-hook 'window-configuration-change-hook #'dirvish--render-attrs nil t)
      buf)))

(defun dirvish--create-parent-windows (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent-path current))
         (parent-dirs ())
         (depth (or (car (dv-layout dv)) 0))
         (i 0))
    (dirvish--init-dired-window dv (selected-window))
    (while (and (< i depth) (not (string= current parent)))
      (cl-incf i)
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent-path current))
      (setq parent (dirvish--get-parent-path parent)))
    (when (> depth 0)
      (cl-loop with parent-width = (nth 1 (dv-layout dv))
               with remain = (- 1 (nth 2 (dv-layout dv)) parent-width)
               with width = (min (/ remain depth) parent-width)
               for level from 1 for (current . parent) in parent-dirs
               for args = `((side . left) (inhibit-same-window . t)
                            (window-width . ,width)
                            (window-parameters . ((no-other-window . t))))
               for b = (dirvish--create-parent-buffer dv parent current level)
               for w = (display-buffer b `(dirvish--display-buffer . ,args)) do
               (with-selected-window w
                 (set-window-fringes nil 1 1) (set-window-dedicated-p w t))))))

(defun dirvish--init-util-buffers (dv)
  "Initialize util buffers for DV."
  (with-current-buffer (dirvish--util-buffer 'preview dv nil t)
    (fundamental-mode) (setq mode-line-format nil header-line-format nil)
    (add-hook 'window-scroll-functions #'dirvish-apply-ansicolor-h nil t))
  (with-current-buffer (dirvish--util-buffer 'header dv)
    (dirvish-prop :dv (dv-name dv))
    (setq cursor-type nil window-size-fixed 'height mode-line-format nil))
  (with-current-buffer (dirvish--util-buffer 'footer dv)
    (dirvish-prop :dv (dv-name dv))
    (setq cursor-type nil window-size-fixed 'height)
    (setq header-line-format nil mode-line-format dirvish--mode-line-fmt)))

(defsubst dirvish--dir-data-getter (dir)
  "Script for DIR data retrieving."
  `(with-temp-buffer
     (let ((hash (make-hash-table))
           (bk ,(and (featurep 'dirvish-vc)
                     `(ignore-errors (vc-responsible-backend ,dir)))))
       (dolist (file (directory-files ,dir t nil t))
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
           (puthash (intern (secure-hash 'md5 file))
                    `(:builtin ,attrs :type ,tp
                               ,@(and state (list :vc-state state))
                               ,@(and git (list :git-msg git)))
                    hash)))
       (prin1 (cons bk hash) (current-buffer)))
     (buffer-substring-no-properties (point-min) (point-max))))

(defun dirvish-dir-data-proc-s (proc _exit)
  "Parse the directory metadata from PROC's output STR."
  (pcase-let ((`(,buf . ,setup) (process-get proc 'meta))
              (`(,vc . ,data) (with-current-buffer (process-buffer proc)
                                (read (buffer-string)))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (maphash (lambda (k v) (puthash k v dirvish--attrs-hash)) data)
        (when setup
          (dirvish-prop :vc-backend vc)
          (run-hooks 'dirvish-setup-hook))
        (unless (derived-mode-p 'wdired-mode) (dirvish-update-body-h)))))
  (delete-process proc)
  (dirvish--kill-buffer (process-buffer proc)))

(cl-defgeneric dirvish-data-for-dir (dir buffer setup)
  "Fetch data for files in DIR, stored locally in BUFFER.
Run `dirvish-setup-hook' afterwards when SETUP is non-nil."
  (let* ((buf (make-temp-name "dir-data-"))
         (c (format "%S" `(message "%s" ,(dirvish--dir-data-getter dir))))
         (proc (make-process :name "dir-data" :connection-type nil :buffer buf
                             :command (list "emacs" "-Q" "-batch" "--eval" c)
                             :sentinel 'dirvish-dir-data-proc-s)))
    (process-put proc 'meta (cons buffer setup))))

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
  (setf (dv-scopes dv) (dirvish--scopes))
  (setf (dv-index-dir dv) (cons (dirvish-prop :root) (current-buffer)))
  (setf (dv-winconf dv) (or (dv-winconf dv) (current-window-configuration)))
  (let* ((layout (dv-layout dv))
         (w-order (and layout (dirvish--window-split-order)))
         (w-args `((preview (side . right) (window-width . ,(nth 2 layout)))
                   (header (side . above) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))
                   (footer (side . below) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))))
         maybe-abnormal)
    (setq dirvish--selected-window (selected-window))
    (dirvish--init-util-buffers dv)
    (when w-order (let ((ignore-window-parameters t)) (delete-other-windows)))
    (dolist (pane w-order)
      (let* ((buf (dirvish--util-buffer pane dv nil (eq pane 'preview)))
             (args (alist-get pane w-args))
             (win (display-buffer buf `(dirvish--display-buffer . ,args))))
        (cond ((eq pane 'preview) (setf (dv-preview-window dv) win))
              (t (set-window-dedicated-p win t) (push win maybe-abnormal)))
        (set-window-buffer win buf)))
    (dirvish--create-parent-windows dv)
    (let ((h-fmt (or (dirvish-prop :cus-header) dirvish--header-line-fmt)))
      (with-current-buffer (dirvish--util-buffer 'header dv)
        (setq header-line-format h-fmt))
      (dirvish--normalize-util-windows maybe-abnormal)
      (unless (dirvish-prop :cached)
        (dirvish-data-for-dir default-directory (current-buffer) t)
        (dirvish-prop :cached t)))
    (setq dirvish--this dv)))

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") 'dirvish-dispatch)
    (define-key map (kbd "q") 'dirvish-quit)
    map)
  "Keymap used in a dirvish buffer.")

(define-derived-mode dirvish-parent-mode fundamental-mode "Dirvish-parent"
  "Major mode for dirvish parent buffers."
  :group 'dirvish :interactive nil)

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Major mode for dirvish buffers."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-quit ()
  "Quit current Dirvish session."
  (interactive)
  (let ((dv (dirvish-curr)) (frame (selected-frame)))
    (dirvish-kill dv)
    (when dirvish-reuse-session (quit-window))
    (unless (eq (selected-frame) frame) (delete-frame frame))))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Let Dirvish take over Dired globally."
  :group 'dirvish :global t
  (let ((ads '((dired-find-file dirvish-find-entry-a :override)
               (dired-dwim-target-next dirvish-dwim-target-next-a :override)
               (dired-noselect dirvish-dired-noselect-a :around)
               (dired-insert-subdir dirvish-insert-subdir-a :after)
               (wdired-change-to-wdired-mode dirvish-wdired-enter-a :after)
               (wdired-change-to-dired-mode dirvish-wdired-exit-a :after)))
        (h-fn #'dirvish-selection-change-h))
    (if dirvish-override-dired-mode
        (progn (pcase-dolist (`(,sym ,fn ,how) ads) (advice-add sym how fn))
               (add-hook 'window-selection-change-functions h-fn))
      (pcase-dolist (`(,sym ,fn) ads) (advice-remove sym fn))
      (add-hook 'window-selection-change-functions h-fn))))

;;;###autoload
(defun dirvish (&optional path)
  "Start a full frame Dirvish session with optional PATH.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `default-directory'."
  (interactive (list (and current-prefix-arg
                          (read-directory-name "Dirvish: "))))
  (setq path (or path default-directory))
  (let ((dv (dirvish-curr)))
    (if (and dv (dv-layout dv))
        (dirvish-find-entry-a path)
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

(defun dirvish-restore-desktop-buffer (_file-name _buffer-name misc-data)
  "Restore a Dirvish buffer with MISC-DATA ."
  (dirvish-override-dired-mode)
  (dired-restore-desktop-buffer nil nil misc-data))

(add-to-list 'desktop-buffer-mode-handlers
             '(dirvish-mode . dirvish-restore-desktop-buffer))

(provide 'dirvish)
;;; dirvish.el ends here
