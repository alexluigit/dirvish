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
(declare-function dirvish-fd-find "dirvish-fd")
(declare-function dirvish-noselect-tramp "dirvish-extras")

;;;; User Options

(defgroup dirvish nil "A better Dired." :group 'dired)

(defcustom dirvish-attributes '(file-size)
  "File attributes showing in file lines.
Dirvish ships with these attributes:

- `subtree-state': A indicator for directory expanding state.
- `all-the-icons': File icons provided by `all-the-icons.el'.
- `nerd-icons': File icons provided by `nerd-icons.el'.
- `vscode-icon': File icons provided by `vscode-icon.el'.
- `collapse': Collapse unique nested paths.
- `git-msg': Append git commit message to filename.
- `vc-state': The version control state at left fringe.
- `file-size': file size or directories file count at right fringe.
- `file-time': Show file modification time before the `file-size'."
  :group 'dirvish :type '(repeat (symbol :tag "Dirvish attribute")))

(defcustom dirvish-preview-dispatchers '(image gif video audio epub archive pdf)
  "List of preview dispatchers.
Each dispatcher in this list handles the validation and preview
content generation for the corresponding filetype.

The default value contains:

- image: preview image files, requires `imagemagick'.
- gif: preview GIF image files with animation.
- video: preview videos files with thumbnail, requires `ffmpegthumbnailer'.
- audio: preview audio files with metadata, requires `mediainfo'.
- epub: preview epub documents, requires `epub-thumbnailer'.
- pdf: preview pdf documents via `pdf-tools'.
- archive: preview archive files such as .tar, .zip, requires `tar' / `unzip'."
  :group 'dirvish :type '(repeat (symbol :tag "Dirvish preview methods")))

(defcustom dirvish-preview-disabled-exts '("iso" "bin" "exe" "gpg" "elc" "eln")
  "Do not preview files end with these extensions."
  :group 'dirvish :type '(repeat (string :tag "File name extension")))

(defcustom dirvish-preview-environment
  '((inhibit-message . t) (non-essential . t) (delay-mode-hooks . t)
    (enable-dir-local-variables . nil) (enable-local-variables . :safe))
  "Variables which are bound for default file preview dispatcher.
Credit: copied from `consult-preview-variables' in `consult.el'."
  :group 'dirvish :type 'alist)

(defcustom dirvish-cache-dir
  (expand-file-name "dirvish/" user-emacs-directory)
  "Preview / thumbnail cache directory for dirvish."
  :group 'dirvish :type 'string)

(defcustom dirvish-default-layout '(1 0.11 0.55)
  "Default layout recipe for fullscreen Dirvish sessions.
The value has the form (DEPTH MAX-PARENT-WIDTH PREVIEW-WIDTH).
DEPTH controls the number of windows displaying parent
directories.  It can be 0 if you don't need the parent
directories.  MAX-PARENT-WIDTH controls the max width allocated
to each parent windows.  PREVIEW-WIDTH controls the width
allocated to preview window.  The default value provides a
1:3:5 (approximately) pane ratio.  Also see
`dirvish-layout-recipes' in `dirvish-extras.el'."
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
  :group 'dirvish
  :type '(choice (const :tag "Do not show the mode line" nil)
                 (const :tag "Display the mode line across directory panes" t)
                 (const :tag "Make the mode line span all panes" global)))

(define-obsolete-variable-alias 'dirvish-header-line-position 'dirvish-use-header-line "Aug 5, 2022")
(defcustom dirvish-use-header-line t
  "Like `dirvish-use-mode-line', but for header line."
  :group 'dirvish :type 'symbol)

(defcustom dirvish-mode-line-height 30
  "Height of Dirvish's mode line.
The value should be a cons cell (H-WIN . H-FRAME), where H-WIN
and H-FRAME represent the height of mode line in single window
state and fullframe state respectively.  If this value is a
integer INT, it is seen as a shorthand for (INT . INT)."
  :group 'dirvish
  :type '(choice interger (cons integer integer)))

(defcustom dirvish-header-line-height 30
  "Like `dirvish-mode-line-height', but for header line."
  :type '(choice interger (cons integer integer)))

(defcustom dirvish-mode-line-format
  '(:left (sort omit symlink) :right (index))
  "Mode line SEGMENTs aligned to left/right respectively.
Here are all the predefined segments you can choose from:

* Basics (from `dirvish-extras')
`path': directory path under the cursor.
`symlink': target of symlink under the cursor.
`sort': sort criteria applied in current buffer.
`omit': a `dired-omit-mode' indicator.
`index': line number / total line count.
`free-space': amount of free space on `default-directory''s file system.
Others are self-explanatory:
`file-size', `file-modes', `file-link-number', `file-user',
`file-group',`file-time',`file-inode-number',`file-device-number'.

* Miscs
`vc-info': version control information (from `dirvish-vc').
`yank': file transfer progress (from `dirvish-yank').

Set it to nil to use the default `mode-line-format'."
  :group 'dirvish :type 'plist)

(defcustom dirvish-header-line-format
  '(:left (path) :right ())
  "Like `dirvish-mode-line-format', but for header line ."
  :group 'dirvish :type 'plist)

(defcustom dirvish-hide-details t
  "Whether to hide detailed information on session startup."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-hide-cursor t
  "Whether to hide cursor in dirvish buffers."
  :group 'dirvish :type 'boolean)

(defconst dirvish-emacs-bin
  (cond
   ((and invocation-directory invocation-name)
    (expand-file-name (concat (file-name-as-directory invocation-directory) invocation-name)))
   ((eq system-type 'darwin)
    "/Applications/Emacs.app/Contents/MacOS/Emacs")
   (t "emacs")))
(defconst dirvish-image-exts '("webp" "wmf" "pcx" "xif" "wbmp" "vtf" "tap" "s1j" "sjp" "sjpg" "s1g" "sgi" "sgif" "s1n" "spn" "spng" "xyze" "rgbe" "hdr" "b16" "mdi" "apng" "ico" "pgb" "rlc" "mmr" "fst" "fpx" "fbs" "dxf" "dwg" "djv" "uvvg" "uvg" "uvvi" "uvi" "azv" "psd" "tfx" "t38" "svgz" "svg" "pti" "btf" "btif" "ktx2" "ktx" "jxss" "jxsi" "jxsc" "jxs" "jxrs" "jxra" "jxr" "jxl" "jpf" "jpx" "jpgm" "jpm" "jfif" "jhc" "jph" "jpg2" "jp2" "jls" "hsj2" "hej2" "heifs" "heif" "heics" "heic" "fts" "fit" "fits" "emf" "drle" "cgm" "dib" "bmp" "hif" "avif" "avcs" "avci" "exr" "fax" "icon" "ief" "jpg" "macp" "pbm" "pgm" "pict" "png" "pnm" "ppm" "ras" "rgb" "tga" "tif" "tiff" "xbm" "xpm" "xwd" "jpe" "jpeg" "cr2" "arw"))
(defconst dirvish-audio-exts '("ape" "stm" "s3m" "ra" "rm" "ram" "wma" "wax" "m3u" "med" "669" "mtm" "m15" "uni" "ult" "mka" "flac" "axa" "kar" "midi" "mid" "s1m" "smp" "smp3" "rip" "multitrack" "ecelp9600" "ecelp7470" "ecelp4800" "vbk" "pya" "lvp" "plj" "dtshd" "dts" "mlp" "eol" "uvva" "uva" "koz" "xhe" "loas" "sofa" "smv" "qcp" "psid" "sid" "spx" "opus" "ogg" "oga" "mp1" "mpga" "m4a" "mxmf" "mhas" "l16" "lbc" "evw" "enw" "evb" "evc" "dls" "omg" "aa3" "at3" "atx" "aal" "acn" "awb" "amr" "ac3" "ass" "aac" "adts" "726" "abs" "aif" "aifc" "aiff" "au" "mp2" "mp3" "mp2a" "mpa" "mpa2" "mpega" "snd" "vox" "wav"))
(defconst dirvish-video-exts '("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs"))
(defconst dirvish-media-exts (append dirvish-image-exts dirvish-video-exts dirvish-audio-exts '("pdf" "epub" "gif")))
(defcustom dirvish-open-with-programs
  (let ((mpv (or (executable-find "mpv") "mpv")))
    `((,dirvish-audio-exts . (,mpv "--profile=builtin-pseudo-gui" "%f"))
      (,dirvish-video-exts . (,mpv "%f"))))
  "Open certain file types using external programs.
The value should be an association list where each element is of
the form (EXTS . (CMD . ARGS)).  EXTS is a list of file name
extensions.  When opening a file whose filename ends with one of
the EXTS using `dired-find-file', a subprocess according to CMD
and its ARGS is issued to open the file externally.  The special
placeholder \"%f\" in the ARGS is replaced by the FILENAME at
runtime.  Set it to nil disables this feature."
  :group 'dirvish
  :type '(alist :key-type ((repeat string) :tag "File extensions")
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

(cl-defgeneric dirvish-clean-cache () "Clean cache for selected files." nil)
(cl-defgeneric dirvish-build-cache () "Build cache for current directory." nil)

(defcustom dirvish-after-revert-hook '(dirvish-clean-cache)
  "Executed after `revert-buffer'."
  :group 'dirvish :type 'hook)

(defcustom dirvish-setup-hook '(dirvish-build-cache)
  "Executed after the Dired buffer is showed up."
  :group 'dirvish :type 'hook)

(defcustom dirvish-find-entry-hook '(dirvish-insert-entry-h)
  "Executed after finding a entry."
  :group 'dirvish :type 'hook)

;;;; Internal variables

(defvar dirvish-scopes '(:frame selected-frame :tab tab-bar--current-tab-index
                                :persp get-current-persp :perspective persp-curr))
(defvar dirvish-libraries
  '((dirvish-widgets  path symlink sort omit index free-space file-link-number
                      file-user file-group file-time file-size file-modes
                      file-inode-number file-device-number
                      audio image gif video epub pdf pdf-preface archive)
    (dirvish-vc       vc-state git-msg vc-diff vc-blame vc-log vc-info)
    (dirvish-icons    all-the-icons nerd-icons vscode-icon)
    (dirvish-collapse collapse)
    (dirvish-subtree  subtree-state)
    (dirvish-yank     yank)))
(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map dired-mode-map)
    (define-key map (kbd "?") 'dirvish-dispatch)
    (define-key map (kbd "q") 'dirvish-quit) map)
  "Keymap used in dirvish buffers.")
(defvar dirvish-redisplay-debounce-timer nil)
(defvar dirvish--history nil)
(defvar dirvish--reset-keywords '(:free-space :content-begin))
(defvar dirvish--selected-window nil)
(defvar dirvish--mode-line-fmt nil)
(defvar dirvish--header-line-fmt nil)
(defvar dirvish--session-hash (make-hash-table))
(defvar dirvish--parent-hash (make-hash-table :test #'equal))
(defvar dirvish--this nil)
(defvar dirvish--available-attrs '())
(defvar dirvish--available-preview-dispatchers '())
(defvar dirvish--working-attrs '())
(defvar dirvish--working-preview-dispathchers '())
(defvar image-dired-thumbnail-buffer)
(defvar-local dirvish--props '())
(defvar-local dirvish--attrs-hash nil)

;;;; Macros

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

(defmacro dirvish-define-attribute (name docstring &rest body)
  "Define a Dirvish attribute NAME.
An attribute contains a pair of predicate/rendering functions
that are being called on `post-command-hook'.  The predicate fn
takes current DV as argument and is executed once.  When it
evaluates to t, the rendering fn runs BODY for every line with
following arguments:

- `f-beg'   from `dired-move-to-filename'
- `f-end'   from `dired-move-to-end-of-filename'
- `f-str'   from (`buffer-substring' F-BEG F-END)
- `f-name'  from `dired-get-filename'
- `f-attrs' from `file-attributes'
- `f-type'  from `file-directory-p' along with `file-symlink-p'
- `l-beg'   from `line-beginning-position'
- `l-end'   from `line-end-position'
- `hl-face' a face that is only passed in on current line

DOCSTRING is the docstring for the attribute.  An optional
`:width' keyword is used to declare the length of the attribute."
  (declare (indent defun) (doc-string 2))
  (let ((ov (intern (format "dirvish-%s-ov" name)))
        (render (intern (format "dirvish-attribute-%s-rd" name)))
        (args '(f-beg f-end f-str f-name f-attrs f-type l-beg l-end hl-face))
        options)
    (while (keywordp (car body)) (dotimes (_ 2) (push (pop body) options)))
    (setq options (reverse options))
    `(progn
       (add-to-list
        'dirvish--available-attrs
        (cons ',name '(,(or (plist-get options :index) 0)
                       ,(or (plist-get options :width) 0)
                       ,(or (plist-get options :when) t)
                       ,render ,ov ,docstring)))
       (defun ,render ,args (ignore ,@args) ,@body))))

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
  (let* ((dp-name (intern (format "dirvish-%s-dp" name)))
         (default-arglist '(file ext preview-window dv))
         (ignore-list (cl-set-difference default-arglist arglist))
         (keywords `(:doc ,docstring)))
    (while (keywordp (car body)) (dotimes (_ 2) (push (pop body) keywords)))
    `(progn
       (add-to-list
        'dirvish--available-preview-dispatchers (cons ',name ',keywords))
       (defun ,dp-name ,default-arglist (ignore ,@ignore-list) ,@body))))

(defmacro dirvish-define-mode-line (name &optional docstring &rest body)
  "Define a mode line segment NAME with BODY and DOCSTRING."
  (declare (indent defun) (doc-string 2))
  (let ((ml-name (intern (format "dirvish-%s-ml" name))))
    `(defun ,ml-name (dv) ,docstring (ignore dv) ,@body)))

;;;; Helpers

(defun dirvish--hide-dired-header ()
  "Hide the Dired header."
  (remove-overlays (point-min) (point) 'dired-header t)
  (save-excursion
    (let* ((beg (goto-char (point-min)))
           (next-file (next-single-property-change beg 'dired-filename))
           (end (or (dirvish-prop :content-begin)
                    (and (not next-file) (point-max))
                    (progn (goto-char next-file) (line-beginning-position))))
           (o (make-overlay beg end)))
      (dirvish-prop :content-begin end)
      (overlay-put o 'dired-header t)
      (overlay-put o 'invisible
                   (cond ((cdr dired-subdir-alist) nil)
                         (dirvish-use-header-line t))))))

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

(defun dirvish--kill-buffer (buffer)
  "Kill BUFFER without side effects."
  (and (buffer-live-p buffer)
       (cl-letf (((symbol-function 'undo-tree-save-history-from-hook) #'ignore)
                 ((symbol-function 'recentf-track-closed-file) #'ignore))
         (let (kill-buffer-query-functions) (kill-buffer buffer)))))

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
  (eq (if (car (dv-layout dv)) (dv-root-window dv) (frame-selected-window))
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

(cl-defstruct (dirvish (:conc-name dv-))
  "Define dirvish data type."
  (type () :documentation "is the (TYPE FIXED-WIDTH DEDICATED ROOT-WIN-FN FILE-OPEN-FN) struct.")
  (layout (cons nil dirvish-default-layout) :documentation "is the working layouts.")
  (ls-switches dired-listing-switches :documentation "is the listing switches.")
  (root-window nil :documentation "is the main window created by ROOT-WINDOW-FN.")
  (scopes () :documentation "are the \"environments\" such as init frame of this session.")
  (preview-buffers () :documentation "holds all file preview buffers in this session.")
  (preview-window nil :documentation "is the window to display preview buffer.")
  (name (cl-gensym) :documentation "is an unique symbol for every session.")
  (winconf (current-window-configuration) :documentation "is the saved window configuration.")
  (index () :documentation "is a (DIR . CORRESPONDING-BUFFER) cons of ROOT-WINDOW.")
  (roots () :documentation "is the list of all INDEXs."))

(defun dirvish--find-reusable (&optional type)
  "Return the first matched reusable session with TYPE."
  (when dirvish-reuse-session
    (cl-loop with scopes = (dirvish--scopes)
             for dv in (hash-table-values dirvish--session-hash)
             when (and (eq type (car (dv-type dv)))
                       (equal (dv-scopes dv) scopes))
             collect dv)))

(defun dirvish-new (&rest args)
  "Create and save a new dirvish struct to `dirvish--session-hash'.
ARGS is a list of keyword arguments for `dirvish' struct."
  (let (slots new)
    (while (keywordp (car args)) (dotimes (_ 2) (push (pop args) slots)))
    (setq new (apply #'make-dirvish (reverse slots)) dirvish--this new)
    (puthash (dv-name new) new dirvish--session-hash)
    (dirvish--check-deps)
    (dirvish--create-root-window new) new))

(defun dirvish-kill (dv)
  "Kill the dirvish instance DV."
  (let ((index (cdr (dv-index dv))))
    (if (not (car (dv-layout dv)))
        (cl-loop for (_d . b) in (dv-roots dv) when
                 (not (get-buffer-window b)) do (kill-buffer b)
                 finally (setf (dv-index dv) (car (dv-roots dv))))
      (when dirvish-use-header-line
        (with-current-buffer index
          (setq header-line-format dirvish--header-line-fmt)))
      (cl-loop for (_d . b) in (dv-roots dv)
               when (not (eq b index)) do (kill-buffer b))
      (when-let ((wconf (dv-winconf dv))) (set-window-configuration wconf)))
    (mapc #'dirvish--kill-buffer (dv-preview-buffers dv))
    (cl-loop for b in (buffer-list) for bn = (buffer-name b) when
             (string-match-p (format " ?\\*Dirvish-.*-%s\\*" (dv-name dv)) bn)
             do (dirvish--kill-buffer b))
    (setq dirvish--parent-hash (make-hash-table :test #'equal))
    (cond (dirvish-reuse-session (setf (dv-winconf dv) nil))
          (t (mapc (pcase-lambda (`(,_ . ,b)) (kill-buffer b)) (dv-roots dv))))
    (setq dirvish--this nil)))

(defun dirvish--create-root-window (dv)
  "Create root window of DV."
  (let* ((fn (or (nth 3 (dv-type dv)) 'frame-selected-window)) (w (funcall fn)))
    (setf (dv-root-window dv) w) w))

(defun dirvish--preview-dps-validate (&optional dps)
  "Check if the requirements of dispatchers DPS are met."
  (cl-loop with dps = (or dps dirvish-preview-dispatchers)
           with res = (prog1 '() (require 'recentf) (require 'ansi-color))
           with fmt = "[Dirvish]: install '%s' executable to preview %s files."
           for dp in (append '(disable) dps '(default))
           for info = (alist-get dp dirvish--available-preview-dispatchers)
           for requirements = (plist-get info :require)
           for met = t
           do (progn (dolist (pkg requirements)
                       (unless (executable-find pkg)
                         (message fmt pkg dp) (setq met nil)))
                     (when met (push (intern (format "dirvish-%s-dp" dp)) res)))
           finally return (reverse res)))

(defun dirvish--attrs-expand (attrs)
  "Expand ATTRS to `dirvish--working-attrs'."
  (sort (cl-loop for attr in attrs
                 for lst = (alist-get attr dirvish--available-attrs)
                 for (idx width pred render ov _) = lst
                 collect (list idx (eval width) pred render ov))
        (lambda (a b) (< (car a) (car b)))))

(defun dirvish--check-deps ()
  "Remove invalid widgets, raise warnings for missing dependencies."
  (cl-loop
   with (m . h) = (cons dirvish-mode-line-format dirvish-header-line-format)
   with (ml-l . ml-r) = (cons (plist-get m :left) (plist-get m :right))
   with (hl-l . hl-r) = (cons (plist-get h :left) (plist-get h :right))
   with feat-reqs = (append dirvish-preview-dispatchers ml-l ml-r hl-l hl-r)
   with attrs = '(hl-line symlink-target)
   for (lib . feat) in dirvish-libraries do
   (let ((m-attr (cl-intersection feat dirvish-attributes))
         (feat-in-lib (cl-intersection feat feat-reqs)))
     (when (or m-attr feat-in-lib) (require lib))
     (and m-attr (setq attrs (append attrs m-attr))))
   finally
   (setf dirvish--mode-line-fmt (dirvish--mode-line-fmt-setter ml-l ml-r))
   (setf dirvish--header-line-fmt (dirvish--mode-line-fmt-setter hl-l hl-r t))
   (setf dirvish--working-preview-dispathchers (dirvish--preview-dps-validate))
   (setf dirvish--working-attrs (dirvish--attrs-expand attrs))))

(defun dirvish--render-attrs-1 (height width pos remote fns ov align-to)
  "HEIGHT WIDTH POS REMOTE FNS OV ALIGN-TO."
  (forward-line (- 0 height))
  (cl-dotimes (_ (* (if (eq major-mode 'dired-mode) 2 5) height))
    (when (eobp) (cl-return))
    (let ((f-beg (dired-move-to-filename))
          (f-end (dired-move-to-end-of-filename t))
          (l-beg (line-beginning-position)) (l-end (line-end-position))
          (f-wid 0) f-str f-name f-attrs f-type hl-face left right)
      (setq hl-face
            (and (eq (or f-beg l-beg) pos) dirvish-hide-cursor 'dirvish-hl-line))
      (when f-beg
        (setq f-str (buffer-substring f-beg f-end)
              f-wid (string-width f-str)
              f-name (concat (dired-current-directory) f-str)
              f-attrs (dirvish-attribute-cache f-name :builtin
                        (unless remote (file-attributes f-name)))
              f-type (dirvish-attribute-cache f-name :type
                       (let ((ch (progn (back-to-indentation) (char-after))))
                         `(,(if (eq ch 100) 'dir 'file) . nil))))
        (unless (get-text-property f-beg 'mouse-face)
          (dired-insert-set-properties l-beg l-end)))
      (cl-loop
       for fn in (if f-beg fns '(dirvish-attribute-hl-line-rd))
       for (k . v) = (funcall fn f-beg f-end f-str f-name
                              f-attrs f-type l-beg l-end hl-face)
       do (pcase k ('ov (overlay-put v ov t))
                 ('left (setq left (concat v left)))
                 ('right (setq right (concat v right))))
       finally
       (prog1 (unless (or left right) (cl-return))
         (let* ((len1 (length right))
                (remain (- width len1
                           (or (get-text-property l-beg 'line-prefix) 0)))
                (len2 (min (length left) (max 0 (- remain f-wid 1))))
                (ovl (make-overlay f-end f-end))
                (r-pos (if (> remain f-wid) l-end
                         (let ((end (+ f-beg remain))
                               (offset (- f-wid (length f-str))))
                           (- end offset))))
                (spec `(space :align-to (- right-fringe ,len1 ,align-to)))
                (spc (propertize " " 'display spec 'face hl-face))
                (ovr (make-overlay r-pos r-pos)))
           (overlay-put ovl 'dirvish-l-end-ov t)
           (overlay-put ovl 'after-string (substring (or left "") 0 len2))
           (overlay-put ovr 'dirvish-r-end-ov t)
           (overlay-put ovr 'after-string (concat spc right))))))
    (forward-line 1)))

(defun dirvish--render-attrs (&optional clear)
  "Render or CLEAR attributes in DV's dirvish buffer."
  (cl-loop with remote = (dirvish-prop :remote) with gui = (dirvish-prop :gui)
           with fns = () with height = (frame-height)
           with remain = (- (window-width) (if gui 1 2))
           for (_ width pred render ov) in dirvish--working-attrs
           do (remove-overlays (point-min) (point-max) ov t)
           when (eval pred `((win-width . ,remain)))
           do (setq remain (- remain width)) (push render fns)
           initially
           (remove-overlays (point-min) (point-max) 'dirvish-l-end-ov t)
           (remove-overlays (point-min) (point-max) 'dirvish-r-end-ov t)
           finally
           (with-silent-modifications
             (unless clear
               (save-excursion
                 (dirvish--render-attrs-1 height remain (point)
                                          remote fns ov (if gui 0 2)))))))

;;;; Advices

(defun dirvish-find-entry-a (&optional entry)
  "Find ENTRY in current dirvish session.
ENTRY can be a filename or a string with format of
`dirvish-fd-bufname' used to query or create a `fd' result
buffer, it defaults to filename under the cursor when it is nil."
  (let* ((entry (or entry (dired-get-filename nil t)))
         (buffer (cond ((string-prefix-p "🔍" entry) (dirvish-fd-find entry))
                       ((file-directory-p entry) (dired-noselect entry))
                       ((string-suffix-p "/" entry)
                        (user-error
                         (concat entry " is not a valid directory"))))))
    (if buffer (switch-to-buffer buffer)
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
          (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
            (if fn (funcall fn) (dirvish-kill dv)))
          (find-file file))))))

(defun dirvish-insert-subdir-a (dirname &rest _)
  "Setup newly inserted subdir DIRNAME for this Dirvish buffer."
  (dirvish--hide-dired-header)
  (dirvish-data-for-dir dirname (current-buffer) nil))

(defun dirvish-wdired-enter-a (&rest _)
  "Advice for `wdired-change-to-wdired-mode'."
  (when dirvish-hide-cursor (dired-move-to-end-of-filename t))
  (setq-local cursor-type 'hollow)
  (when (boundp 'evil-normal-state-cursor)
    (setq-local evil-normal-state-cursor 'hollow))
  (dirvish--render-attrs 'clear)
  (remove-hook 'window-configuration-change-hook #'dirvish-winconf-change-h t)
  (remove-hook 'post-command-hook #'dirvish-update-body-h t))

(defun dirvish-thumb-buf-a (fn)
  "Advice for FN `image-dired-create-thumbnail-buffer'."
  (when-let ((dv dirvish--this) ((dv-preview-window dv)))
    (dirvish--init-session dv)
    (with-selected-window (dv-preview-window dv)
      (switch-to-buffer image-dired-thumbnail-buffer)))
  (let ((buf (funcall fn))
        (fun (lambda () (let ((buf (get-text-property
                               (point) 'associated-dired-buffer)))
                     (and (buffer-live-p buf)
                          (with-current-buffer buf (dirvish--render-attrs)))))))
    (with-current-buffer buf (add-hook 'post-command-hook fun nil t)) buf))

(defun dirvish-dired-noselect-a (fn dir &optional flags)
  "Return buffer for DIR with FLAGS, FN is `dired-noselect'."
  (let* ((key (file-name-as-directory (expand-file-name dir)))
         (this dirvish--this)
         (dv (if (and this (eq this-command 'dired-other-frame)) (dirvish-new)
               (or this (car (dirvish--find-reusable)) (dirvish-new))))
         (bname buffer-file-name)
         (remote (file-remote-p dir))
         (flags (or flags (dv-ls-switches dv)))
         (buffer (alist-get key (dv-roots dv) nil nil #'equal))
         (new-buffer-p (not buffer)))
    (if this (set-window-dedicated-p nil nil) (setcar (dv-layout dv) nil))
    (when new-buffer-p
      (if (not remote)
          (let ((dired-buffers nil)) ; disable reuse from dired
            (setq buffer (apply fn (list dir flags))))
        (require 'dirvish-extras)
        (setq buffer (dirvish-noselect-tramp fn dir flags remote)))
      (with-current-buffer buffer (dirvish-init-dired-buffer))
      (push (cons key buffer) (dv-roots dv))
      (push (cons key buffer) dired-buffers))
    (with-current-buffer buffer
      (cond (new-buffer-p nil)
            ((and (not remote) (not (equal flags dired-actual-switches)))
             (dired-sort-other flags))
            ((eq dired-auto-revert-buffer t) (revert-buffer))
	    ((functionp dired-auto-revert-buffer)
	     (when (funcall dired-auto-revert-buffer dir) (revert-buffer))))
      (dirvish-prop :dv (dv-name dv))
      (dirvish-prop :gui (display-graphic-p))
      (dirvish-prop :remote remote)
      (dirvish-prop :root key)
      (when bname (dired-goto-file bname))
      (setf (dv-index dv) (cons key buffer))
      (run-hook-with-args 'dirvish-find-entry-hook key buffer)
      buffer)))

;;;; Hooks

(defun dirvish-apply-ansicolor-h (_win pos)
  "Update dirvish ansicolor in preview window from POS."
  (ansi-color-apply-on-region
   (goto-char pos) (progn (forward-line (frame-height)) (point))))

(defun dirvish-update-body-h (&optional force)
  "Update UI of current Dirvish.
When FORCE, ensure the preview get refreshed."
  (when-let ((dv (dirvish-curr)))
    (cond ((not dirvish-hide-cursor))
          ((eobp) (forward-line -1))
          ((cdr dired-subdir-alist))
          ((and (bobp) dirvish-use-header-line)
           (goto-char (dirvish-prop :content-begin))))
    (when dirvish-hide-cursor (dired-move-to-filename))
    (dirvish--render-attrs)
    (when-let ((filename (dired-get-filename nil t)))
      (dirvish-prop :index filename)
      (let ((h-buf (dirvish--util-buffer 'header dv t))
            (f-buf (dirvish--util-buffer 'footer dv t))
            (last-index (dirvish-prop :last-index)))
        (dirvish-prop :last-index filename)
        (dirvish-debounce nil
          (if (not (car (dv-layout dv)))
              (and (< emacs-major-version 29) (force-mode-line-update))
            (when (and dirvish-use-mode-line (buffer-live-p f-buf))
              (with-current-buffer f-buf (force-mode-line-update)))
            (when (and dirvish-use-header-line (buffer-live-p h-buf))
              (with-current-buffer h-buf (force-mode-line-update)))
            (when (or force (not (equal last-index filename)))
              (dirvish--preview-update dv filename))))))))

(defun dirvish-insert-entry-h (entry buffer)
  "Add ENTRY or BUFFER name to `dirvish--history'."
  (let ((entry (if (string-prefix-p "🔍" entry)
                   (buffer-name buffer) entry)))
    (setq dirvish--history (seq-take (push entry dirvish--history) 200))))

(defun dirvish-kill-buffer-h ()
  "Remove buffer from session's buffer list."
  (when-let ((dv (dirvish-curr)) (buf (current-buffer)))
    (let ((win (get-buffer-window buf)))
      (when (window-live-p win) (set-window-dedicated-p win nil)))
    (setf (dv-roots dv) (cl-remove-if (lambda (i) (eq (cdr i) buf)) (dv-roots dv)))
    (unless (dv-roots dv)
      (when-let ((layout (car (dv-layout dv)))
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
             (switch-to-buffer b)
             (dirvish--init-session dv)))
          ((active-minibuffer-window))
          (t (setq dirvish--this dv)))
    (setq dirvish--selected-window w)))

(defun dirvish-winconf-change-h ()
  "Restore hidden sessions on buffer switching."
  (let ((dv (dirvish-curr)))
    (setf (dv-root-window dv) (get-buffer-window (cdr (dv-index dv))))
    (dirvish-update-body-h 'force-preview-update)))

(defun dirvish-winbuf-change-h (frame-or-window)
  "Rebuild layout once buffer in FRAME-OR-WINDOW changed."
  (let ((win (frame-selected-window frame-or-window)))
    (with-current-buffer (window-buffer win)
      (when-let ((dv (dirvish-curr))) (dirvish--init-session dv)))))

(defun dirvish-tab-new-post-h (_tab)
  "Do not reuse sessions from other tabs."
  (setq dirvish--this nil))

;;;; Preview

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
  (cl-letf (((symbol-function 'recentf-track-opened-file) #'ignore)
            ((symbol-function 'undo-tree-save-history-from-hook) #'ignore)
            ((symbol-function 'flycheck-mode-on-safe) #'ignore))
    (let* ((vc-follow-symlinks t)
           (vars (mapcar (pcase-lambda (`(,k . ,v))
                           (list k v (default-value k) (symbol-value k)))
                         dirvish-preview-environment))
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
           (let* ((script `(with-current-buffer
                               (progn (setq insert-directory-program
                                            ,insert-directory-program)
                                      (dired-noselect ,file "-AlGh"))
                             (buffer-string)))
                  (cmd (format "%S" `(message "\n%s" ,script))))
             `(dired . (,dirvish-emacs-bin "-Q" "-batch" "--eval" ,cmd))))
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
                     (list (cons (car (dv-index dv)) (point-min-marker)))
                     font-lock-defaults
                     '(dired-font-lock-keywords t nil nil beginning-of-line))
         (font-lock-mode 1)
         (run-hooks 'dirvish-directory-view-mode-hook)))))
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

(defun dirvish--preview-update (dv index)
  "Update preview content of INDEX for DV."
  (when-let* ((window (dv-preview-window dv))
              ((window-live-p window))
              (orig-bufs (buffer-list))
              (ext (downcase (or (file-name-extension index) "")))
              (buf (cl-loop for fn in dirvish--working-preview-dispathchers
                            for rcp = (funcall fn index ext window dv) thereis
                            (and rcp (dirvish-preview-dispatch rcp dv)))))
    (setq-local other-window-scroll-buffer buf)
    (set-window-buffer window buf)
    (unless (memq buf orig-bufs) (push buf (dv-preview-buffers dv)))))

;;;; Builder

(dirvish-define-attribute hl-line
  "Highlight current line.
This attribute is enabled when `dirvish-hide-cursor' is non-nil."
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end))))
      (overlay-put ov 'face hl-face) `(ov . ,ov))))

(dirvish-define-attribute symlink-target
  "Hide symlink target."
  :when (or (eq major-mode 'dirvish-directory-view-mode)
            (and dired-hide-details-mode
                 (default-value 'dired-hide-details-hide-symlink-targets)))
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end)))
      (overlay-put ov 'invisible t) `(ov . ,ov))))

(defun dirvish--mode-line-fmt-setter (left right &optional header)
  "Set the `dirvish--mode-line-fmt'.
LEFT and RIGHT are segments aligned to left/right respectively.
If HEADER, set the `dirvish--header-line-fmt' instead."
  (cl-labels ((expand (segments)
                (cl-loop for s in segments collect
                         (if (stringp s) s
                           `(:eval (,(intern (format "dirvish-%s-ml" s)) (dirvish-curr))))))
              (get-font-scale ()
                (let* ((face (if header 'header-line 'mode-line-inactive))
                       (defualt (face-attribute 'default :height))
                       (ml-height (face-attribute face :height)))
                  (cond ((floatp ml-height) ml-height)
                        ((integerp ml-height) (/ (float ml-height) defualt))
                        (t 1)))))
    `((:eval
       (let* ((dv (dirvish-curr))
              (buf (and (car (dv-layout dv)) (cdr (dv-index dv))))
              (scale ,(get-font-scale))
              (win-width (floor (/ (window-width) scale)))
              (str-l (format-mode-line
                      ',(or (expand left) mode-line-format) nil nil buf))
              (str-r (format-mode-line ',(expand right) nil nil buf))
              (len-r (string-width str-r)))
         (concat
          (dirvish--bar-image (car (dv-layout dv)) ,header)
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
    (cond ((bound-and-true-p evil-normal-state-cursor)
           (setq-local evil-normal-state-cursor '(bar . 0)))
          ((bound-and-true-p meow-cursor-type-default)
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

(defun dirvish-revert (&optional ignore-auto _noconfirm)
  "Reread the Dirvish buffer.
When IGNORE-AUTO, refresh file attributes as well.
Dirvish sets `revert-buffer-function' to this function."
  (dirvish-prop :old-index (dired-get-filename nil t))
  (dolist (keyword dirvish--reset-keywords) (dirvish-prop keyword nil))
  (dired-revert)
  (dirvish--hide-dired-header)
  (when ignore-auto ; meaning it is called interactively from user
    (setq-local dirvish--attrs-hash (make-hash-table))
    (dirvish-data-for-dir default-directory (current-buffer) t))
  (run-hooks 'dirvish-after-revert-hook))

(defun dirvish-init-dired-buffer ()
  "Initialize a Dired buffer for dirvish."
  (when (file-remote-p default-directory)
    (setq-local dirvish--working-preview-dispathchers '(dirvish-tramp-dp)))
  (use-local-map dirvish-mode-map)
  (dirvish--hide-cursor)
  (dirvish--hide-dired-header)
  (and dirvish-hide-details (dired-hide-details-mode t))
  (setq-local dirvish--attrs-hash (or dirvish--attrs-hash (make-hash-table))
              revert-buffer-function #'dirvish-revert
              tab-bar-new-tab-choice "*scratch*"
              dired-hide-details-hide-symlink-targets nil
              dired-kill-when-opening-new-dired-buffer nil)
  (add-hook 'window-buffer-change-functions #'dirvish-winbuf-change-h nil t)
  (add-hook 'window-configuration-change-hook #'dirvish-winconf-change-h nil t)
  (add-hook 'post-command-hook #'dirvish-update-body-h nil t)
  (add-hook 'kill-buffer-hook #'dirvish-kill-buffer-h nil t)
  (set-buffer-modified-p nil))

(defun dirvish--create-parent-buffer (dv dir index level)
  "Create parent buffer at DIR in DV selecting file INDEX.
LEVEL is the depth of current window."
  (let ((index (directory-file-name index))
        (buf (dirvish--util-buffer (format "parent-%s" level) dv nil t))
        (str (or (gethash dir dirvish--parent-hash)
                 (let ((flags dired-actual-switches))
                   (with-temp-buffer (dired-insert-directory dir flags)
                                     (buffer-string)))))
        (attrs (append
                '(hl-line symlink-target)
                (cond ((memq 'all-the-icons dirvish-attributes) '(all-the-icons))
                      ((memq 'nerd-icons dirvish-attributes) '(nerd-icons))
                      ((memq 'vscode-icon dirvish-attributes) '(vscode-icon))))))
    (with-current-buffer buf
      (dirvish-directory-view-mode)
      (dirvish-prop :dv (dv-name dv))
      (dirvish-prop :remote (file-remote-p dir))
      (puthash dir str dirvish--parent-hash)
      (erase-buffer)
      (setq mode-line-format nil header-line-format nil)
      (save-excursion (insert str))
      (setq-local dired-subdir-alist (list (cons dir (point-min-marker)))
                  font-lock-defaults
                  '(dired-font-lock-keywords t nil nil beginning-of-line))
      (font-lock-mode 1)
      (dired-goto-file-1 (file-name-nondirectory index) index (point-max))
      (dirvish--hide-cursor)
      (setq-local dirvish--attrs-hash (make-hash-table)
                  dirvish--working-attrs (dirvish--attrs-expand attrs))
      (dirvish--render-attrs) buf)))

(defun dirvish--create-parent-windows (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent-path current))
         (parent-dirs ())
         (depth (or (caar (dv-layout dv)) 0))
         (i 0))
    (dirvish--setup-mode-line (car (dv-layout dv)))
    (when-let (fixed (nth 1 (dv-type dv))) (setq window-size-fixed fixed))
    (set-window-dedicated-p
     nil (and (or (car (dv-layout dv)) (nth 2 (dv-type dv))) t))
    (set-window-fringes nil 1 1)
    (while (and (< i depth) (not (string= current parent)))
      (cl-incf i)
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent-path current))
      (setq parent (dirvish--get-parent-path parent)))
    (when (> depth 0)
      (cl-loop with layout = (car (dv-layout dv)) with parent-width = (nth 1 layout)
               with remain = (- 1 (nth 2 layout) parent-width)
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
                             :command (list dirvish-emacs-bin "-Q" "-batch" "--eval" c)
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

(defun dirvish--init-session (dv)
  "Build layout for Dirvish session DV."
  (setf (dv-scopes dv) (dirvish--scopes))
  (setf (dv-index dv) (cons (dirvish-prop :root) (current-buffer)))
  (setf (dv-winconf dv) (or (dv-winconf dv) (current-window-configuration)))
  (let* ((layout (car (dv-layout dv)))
         (w-args `((preview (side . right) (window-width . ,(nth 2 layout)))
                   (header (side . above) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))
                   (footer (side . below) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))))
         (w-order (and layout (dirvish--window-split-order))) util-windows)
    (setq dirvish--selected-window (selected-window))
    (dirvish--init-util-buffers dv)
    (when w-order (let ((ignore-window-parameters t)) (delete-other-windows)))
    (dolist (pane w-order)
      (let* ((buf (dirvish--util-buffer pane dv nil (eq pane 'preview)))
             (args (alist-get pane w-args))
             (win (display-buffer buf `(dirvish--display-buffer . ,args))))
        (cond ((eq pane 'preview) (setf (dv-preview-window dv) win))
              (t (set-window-dedicated-p win t) (push win util-windows)))
        (set-window-buffer win buf)))
    (dirvish--create-parent-windows dv)
    (let ((h-fmt (or (dirvish-prop :cus-header) dirvish--header-line-fmt)))
      (with-current-buffer (dirvish--util-buffer 'header dv)
        (setq header-line-format h-fmt))
      (when (and (display-graphic-p) (> emacs-major-version 28))
        (let ((window-safe-min-height 0) (window-resize-pixelwise t))
          (dolist (win util-windows) (fit-window-to-buffer win 2 1))))
      (unless (dirvish-prop :cached)
        (dirvish-data-for-dir default-directory (current-buffer) t)
        (dirvish-prop :cached t)))
    (setq dirvish--this dv)))

(defun dirvish--reuse-or-create (path layout)
  "Find PATH in a dirvish session and set its layout with LAYOUT."
  (let ((dir (or path default-directory))
        (dv (or dirvish--this (car (dirvish--find-reusable)))))
    (cond (dv (with-selected-window (dirvish--create-root-window dv)
                (setcar (dv-layout dv) layout)
                (setq dirvish--this dv)
                (dirvish-find-entry-a
                 (if (or path (not (eq dirvish-reuse-session 'resume))) dir
                   (car (dv-index dv))))
                (dirvish--init-session dv)))
          (t (dirvish-new :layout (cons layout dirvish-default-layout))
             (dirvish-find-entry-a dir)))))

(define-derived-mode dirvish-directory-view-mode
  fundamental-mode "Dirvish-directory-view"
  "Major mode for dirvish parent buffers."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-quit ()
  "Quit current Dirvish session.
If the session is a full-framed one, the window layout is
restored.  If `dirvish-reuse-session' is nil, all Dired buffers
in the session are killed, otherwise only the invisible Dired
buffers within the session are killed and the Dired buffer(s) in
the selected window are buried."
  (interactive)
  (let ((dv (dirvish-curr)) (ct 0) (lst (window-list))
        (win (selected-window)) (frame (selected-frame)))
    (dirvish-kill dv)
    (while (and (dirvish-curr) (eq (selected-window) win)
                (<= (cl-incf ct) (length lst)))
      (quit-window))
    (unless (eq (selected-frame) frame) (delete-frame frame))))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Let Dirvish take over Dired globally."
  :group 'dirvish :global t
  (let ((ads '((dired-find-file dirvish-find-entry-a :override)
               (dired-noselect dirvish-dired-noselect-a :around)
               (dired-insert-subdir dirvish-insert-subdir-a :after)
               (image-dired-create-thumbnail-buffer dirvish-thumb-buf-a :around)
               (wdired-change-to-wdired-mode dirvish-wdired-enter-a :after)
               (wdired-change-to-dired-mode dirvish-init-dired-buffer :after)))
        (sel-ch #'dirvish-selection-change-h)
        (tab-post #'dirvish-tab-new-post-h))
    (if dirvish-override-dired-mode
        (progn (pcase-dolist (`(,sym ,fn ,how) ads) (advice-add sym how fn))
               (add-hook 'window-selection-change-functions sel-ch)
               (add-hook 'tab-bar-tab-post-open-functions tab-post))
      (pcase-dolist (`(,sym ,fn) ads) (advice-remove sym fn))
      (remove-hook 'window-selection-change-functions sel-ch)
      (remove-hook 'tab-bar-tab-post-open-functions tab-post))))

;;;###autoload
(defun dirvish (&optional path)
  "Start a full frame Dirvish session with optional PATH.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `default-directory'."
  (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
  (dirvish--reuse-or-create path dirvish-default-layout))

;;;###autoload
(defun dirvish-dwim (&optional path)
  "Start a fullframe session only when `one-window-p'.
If called with \\[universal-arguments], prompt for PATH,
otherwise it defaults to `default-directory'.
If `one-window-p' returns nil, open PATH using regular Dired."
  (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
  (dirvish--reuse-or-create
   path (if dirvish--this (car (dv-layout dirvish--this))
          (and (one-window-p) dirvish-default-layout))))

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
   ("c" "Dired cheatsheet"       dirvish-dired-cheatsheet)])

(provide 'dirvish)
;;; dirvish.el ends here
