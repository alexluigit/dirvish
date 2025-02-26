;;; dirvish.el --- A modern file manager based on dired mode -*- lexical-binding: t -*-
;; Copyright (C) 2021-2025 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.1.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1"))

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
(require 'cl-lib)
(eval-when-compile (require 'project))
(declare-function ansi-color-apply-on-region "ansi-color")
(declare-function dirvish-fd-find "dirvish-fd")
(declare-function dirvish-tramp-noselect "dirvish-tramp")

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

(defcustom dirvish-preview-dispatchers
  `(,(if (memq system-type '(ms-dos windows-nt)) 'video-mtn 'video)
    image gif audio epub archive pdf dired)
  "List of preview dispatchers.
Each dispatcher in this list handles the validation and preview
content generation for the corresponding filetype.

The default value contains:

- image: preview image files, requires `imagemagick'.
- gif: preview GIF image files with animation.
- video: preview videos files with thumbnail.
    requires `ffmpegthumbnailer' on Linux/macOS | `mtn' on Windows
- audio: preview audio files with metadata, requires `mediainfo'.
- epub: preview epub documents, requires `epub-thumbnailer'.
- pdf: preview pdf documents via `pdf-tools'.
- archive: preview archive files such as .tar, .zip, requires `tar' / `unzip'.
- dired: preview directories using `emacs --batch'."
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
  "Default layout recipe for Dirvish sessions.
The value has the form (DEPTH MAX-PARENT-WIDTH PREVIEW-WIDTH).

- DEPTH controls the number of windows displaying parent directories, it
can be 0 if you don't need the parent directories.
- MAX-PARENT-WIDTH controls the max width allocated to each parent windows.
- PREVIEW-WIDTH controls the width allocated to preview window.

The default value provides a 1:3:5 (approximately) pane ratio.  Also see
`dirvish-layout-recipes' in `dirvish-extras.el'.

Alternatively, set this to nil to hide both the parent and preview
windows.  In this case, \\='(1 0.11 0.55) will be used as the fallback
full-frame recipe.  This is useful when you want to use `dirvish'
exclusively for directory entries without `dired' or similar commands,
and want to prevent the preview from appearing at startup.  You can
still use `dirvish-layout-toggle' or `dirvish-layout-switch' to display
the full-frame layout when file previews are needed."
  :group 'dirvish
  :type '(choice (const :tag "no default layout" nil)
                 (list (integer :tag "number of parent windows")
                       (float :tag "max width of parent windows")
                       (float :tag "width of preview windows"))))

(defface dirvish-hl-line
  '((t :inherit highlight :extend t))
  "Face used for Dirvish line highlighting."
  :group 'dirvish)

(defface dirvish-inactive
  '((t :inherit shadow))
  "Face used for inactive Dirvish windows."
  :group 'dirvish)

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
  :type '(choice integer (cons integer integer)))

(defcustom dirvish-header-line-height 30
  "Like `dirvish-mode-line-height', but for header line."
  :type '(choice integer (cons integer integer)))

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
  "Whether to enable `dired-hide-details-mode' in Dirvish buffers.
When sets to t, it is enabled for all Dirvish buffers.

Alternatively, the value can be a list of symbols to instruct Dirvish in
what contexts it should be enabled.  The accepted values are:
 - `dired':        when opening a directory using `dired-*' commands.
 - `dirvish':      when opening full-frame Dirvish.
 - `dirvish-fd':   when the buffer is create by `dirvish-fd*' commands.
 - `dirvish-side': when opening Dirvish in the sidebar."
  :group 'dirvish
  :type '(choice (boolean :tag "Apply to all Dirvish buffers")
                 (repeat :tag "Apply to a list of buffer types: 'dired, 'dirvish, 'dirvish-fd or 'dirvish-side" symbol)))

(defcustom dirvish-hide-cursor t
  "Whether to hide cursor in dirvish buffers.
Works all the same as `dirvish-hide-details' but for cursor."
  :group 'dirvish
  :type '(choice (boolean :tag "Apply to all Dirvish buffers")
                 (repeat :tag "Apply to a list of buffer types: 'dired, 'dirvish, 'dirvish-fd or 'dirvish-side" symbol)))

(defcustom dirvish-preview-dired-sync-omit nil
  "If non-nil, `dired' preview buffers sync `dired-omit-mode' from root window.
Notice that it only take effects on the built-in `dired' preview dispatcher."
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
  :group 'dirvish :type '(alist :key-type (repeat :tag "File extensions" string)
                                :value-type (repeat :tag "External command and args" string)))

(defcustom dirvish-reuse-session t
  "Whether to keep the latest session index buffer for later reuse.
The valid values are:
- t:      keep index buffer on both `dirvish-quit' and file open
- `resume': keep and resume to the index when using `dirvish' w/o specify a path
- `quit':   only keep index after `dirvish-quit'
- `open':   only keep index after open a file
- nil:    never keep any index buffers on `dirvish-quit' or open files"
  :group 'dirvish :type '(choice (const :tag "keep index buffer on both `dirvish-quit' and file open" t)
                                 (const :tag "keep and resume to the index when using `dirvish' w/o specify a path" resume)
                                 (const :tag "only keep index after `dirvish-quit'" quit)
                                 (const :tag "only keep index after open a file" open)
                                 (const :tag "never keep any index buffer" nil)))

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

(defvar dirvish-scopes
  '(:frame selected-frame :tab tab-bar--current-tab-index :persp persp-curr))
(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map dired-mode-map)
    (define-key map (kbd "q") 'dirvish-quit) map)
  "Keymap used in dirvish buffers.")
(defvar dirvish--libraries
  '((dirvish-vc       vc-state git-msg vc-diff vc-blame vc-log vc-info)
    (dirvish-icons    all-the-icons nerd-icons vscode-icon)
    (dirvish-collapse collapse)
    (dirvish-subtree  subtree-state)
    (dirvish-yank     yank)))
(defvar dirvish-redisplay-debounce-timer nil)
(defvar dirvish--history nil)
(defvar dirvish--reset-keywords '(:free-space :content-begin))
(defvar dirvish--selected-window nil)
(defvar dirvish--session-hash (make-hash-table :test #'equal))
(defvar dirvish--parent-hash (make-hash-table :test #'equal))
(defvar dirvish--available-attrs '())
(defvar dirvish--available-preview-dispatchers '())
(defvar image-dired-thumbnail-buffer)
(defvar server-buffer-clients)
(defvar-local dirvish--props '())
(defvar-local dirvish--attrs-hash nil)

;;;; Macros

(defmacro dirvish-prop (prop &rest body)
  "Retrieve PROP from `dirvish--props'.
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
  "Run BODY after undedicating window, restore dedication afterwards."
  (declare (debug (&rest form)))
  `(progn
     (let ((dedicated (window-dedicated-p)))
       (set-window-dedicated-p nil nil)
       (prog1 ,@body (set-window-dedicated-p nil dedicated)))))

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
  `(let* ((md5 (secure-hash 'md5 ,file))
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
    `(defun ,ml-name () ,docstring ,@body)))

;;;; Helpers

(defsubst dirvish-curr ()
  "Return Dirvish session attached to current buffer, if there is any."
  (gethash (dirvish-prop :dv) dirvish--session-hash))

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
              (pj-root (project-root pj)))
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

(defun dirvish--change-selected (&rest _)
  "Record `dirvish--selected-window'."
  (setq dirvish--selected-window (frame-selected-window)))

(defun dirvish--selected-p (&optional dv)
  "Return t if session DV (defaults to `dirvish-curr') is selected."
  (when-let* ((dv (or dv (dirvish-curr))))
    (if (dv-curr-layout dv) (eq (dv-root-window dv) dirvish--selected-window)
      (eq (frame-selected-window) dirvish--selected-window))))

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

(defun dirvish--util-buffer (type &optional dv no-create inhibit-hiding)
  "Return session DV's utility buffer of TYPE (defaults to `temp').
If NO-CREATE is non-nil, do not create the buffer.
If INHIBIT-HIDING is non-nil, do not hide the buffer."
  (let* ((id (if dv (format "-%s*" (dv-id dv)) "*"))
         (name (format "%s*Dirvish-%s%s" (if inhibit-hiding "" " ") type id)))
    (if no-create (get-buffer name) (get-buffer-create name))))

(defun dirvish--make-proc (form sentinel buffer-or-name &rest puts)
  "Make process for shell or batch FORM in BUFFER-OR-NAME.
Set process's SENTINEL and PUTS accordingly."
  (let* ((buf (or buffer-or-name (make-temp-name "*dirvish-batch*")))
         (print-length nil) (print-level nil)
         (cmd (if (stringp (car form)) form
                (list dirvish-emacs-bin
                      "-Q" "-batch" "--eval" (prin1-to-string form))))
         (proc (make-process :name "dirvish" :connection-type nil :buffer buf
                             :command cmd :sentinel sentinel)))
    (while-let ((k (pop puts)) (v (pop puts))) (process-put proc k v))))

;;;; Session Struct

(cl-defstruct (dirvish (:conc-name dv-))
  "Define dirvish session (`DV' for short) struct."
  (id (format-time-string "%D|%T")    :documentation "is the created time stamp of DV, used as its unique id.")
  (type 'default                      :documentation "is the type of DV.")
  (root-window ()                     :documentation "is the root/main window of DV.")
  (dedicated ()                       :documentation "passes to `set-window-dedicated-p' for ROOT-WINDOW.")
  (size-fixed ()                      :documentation "passes to `window-size-fixed' for ROOT-WINDOW.")
  (root-window-fn ()                  :documentation "is a function used to create the ROOT-WINDOW for DV.")
  (open-file-fn ()                    :documentation "is a function used to open file under the cursor.")
  (curr-layout ()                     :documentation "is the working layout recipe of DV.")
  (ff-layout dirvish-default-layout   :documentation "is a full-frame layout recipe.")
  (reuse ()                           :documentation "indicates if DV has been reused.")
  (ls-switches dired-listing-switches :documentation "is the directory listing switches.")
  (mode-line ()                       :documentation "is the `mode-line-format' used by this DV.")
  (header-line ()                     :documentation "is the `header-line-format' used by DV.")
  (preview-dispatchers ()             :documentation "is the working preview methods of DV.")
  (attributes ()                      :documentation "is the working attributes of DV.")
  (preview-buffers ()                 :documentation "holds all file preview buffers of DV.")
  (preview-window ()                  :documentation "is the window to display preview buffer.")
  (winconf ()                         :documentation "is a saved window configuration.")
  (index ()                           :documentation "is the current (cwd-str . buffer-obj) cons within ROOT-WINDOW.")
  (roots ()                           :documentation "is all the history INDEX entries in DV."))

(defun dirvish--new (&rest args)
  "Create and save a new dirvish struct to `dirvish--session-hash'.
ARGS is a list of keyword arguments for `dirvish' struct."
  (let (slots new)
    (while (keywordp (car args)) (dotimes (_ 2) (push (pop args) slots)))
    (setq new (apply #'make-dirvish (reverse slots)))
    ;; ensure we have a fallback fullframe layout
    (unless dirvish-default-layout (setf (dv-ff-layout new) '(1 0.11 0.55)))
    (puthash (dv-id new) new dirvish--session-hash)
    (dirvish--check-dependencies new)
    (dirvish--create-root-window new) new))

(defun dirvish--get-session (&optional key val)
  "Return the first matched session has KEY of VAL."
  (setq key (or key 'type) val (or val 'default))
  (cl-loop for dv being the hash-values of dirvish--session-hash
           for b = (cdr (dv-index dv))
           with (fr tab psp) = (cl-loop for (_ v) on dirvish-scopes by 'cddr
                                        collect (and (functionp v) (funcall v)))
           if (or (null b) ; newly created session
                  (and (buffer-live-p b)
                       (eq (with-current-buffer b (dirvish-prop :tab)) tab)
                       (eq (with-current-buffer b (dirvish-prop :frame)) fr)
                       (eq (with-current-buffer b (dirvish-prop :persp)) psp)))
           if (let ((res (funcall (intern (format "dv-%s" key)) dv)))
                (cond ((eq val 'any) res)
                      ((eq key 'roots) (memq val (mapcar #'cdr res)))
                      (t (equal val res))))
           return dv))

(defun dirvish--clear-session (dv &optional from-quit)
  "Reset DV's slot and kill its buffers.
FROM-QUIT is used to signify the calling command."
  (let ((index (cdr (dv-index dv))))
    (when (dirvish-prop :fringe)
      (set-window-fringes nil (frame-parameter nil 'left-fringe)))
    (if (not (dv-curr-layout dv))
        (cl-loop for (_d . b) in (dv-roots dv)
                 when (and (not (get-buffer-window b))
                           (not (with-current-buffer b server-buffer-clients)))
                 do (kill-buffer b)
                 finally (setf (dv-index dv) (car (dv-roots dv))))
      (cl-loop for (_d . b) in (dv-roots dv)
               when (not (eq b index)) do (kill-buffer b))
      (when-let* ((wconf (dv-winconf dv))) (set-window-configuration wconf)))
    (mapc #'dirvish--kill-buffer (dv-preview-buffers dv))
    (cl-loop for b in (buffer-list) for bn = (buffer-name b) when
             (string-match-p (format " ?\\*Dirvish-.*-%s\\*" (dv-id dv)) bn)
             do (dirvish--kill-buffer b))
    (setq dirvish--parent-hash (make-hash-table :test #'equal))
    (setf (dv-winconf dv) nil)
    (when (or (null dirvish-reuse-session)
              (eq dirvish-reuse-session (if from-quit 'open 'quit)))
      (mapc (pcase-lambda (`(,_ . ,b)) (kill-buffer b)) (dv-roots dv)))))

(defun dirvish--create-root-window (dv)
  "Create root window of DV."
  (let* ((fn (or (dv-root-window-fn dv) #'frame-selected-window))
         (w (funcall fn)))
    (setf (dv-root-window dv) w) w))

(defun dirvish--preview-dps-validate (&optional dps)
  "Check if the requirements of dispatchers DPS are met."
  (cl-loop with dps = (or dps dirvish-preview-dispatchers)
           with res = (prog1 '() (require 'recentf) (require 'ansi-color))
           with fmt = "[Dirvish]: install '%s' executable to preview %s files."
           for dp in (append '(disable) dps '(fallback))
           for info = (alist-get dp dirvish--available-preview-dispatchers)
           for requirements = (plist-get info :require)
           for met = t
           do (progn (dolist (pkg requirements)
                       (unless (executable-find pkg)
                         (message fmt pkg dp) (setq met nil)))
                     (when met (push (intern (format "dirvish-%s-dp" dp)) res)))
           finally return (reverse res)))

(defun dirvish--attrs-expand (attrs)
  "Expand ATTRS from `dirvish--available-attrs'."
  (sort (cl-loop for attr in attrs
                 for lst = (alist-get attr dirvish--available-attrs)
                 for (idx width pred render ov _) = lst
                 collect (list idx (eval width) pred render ov))
        (lambda (a b) (< (car a) (car b)))))

(defun dirvish--check-dependencies (dv)
  "Require necessary extensions for DV, raise warnings for missing executables."
  (cl-loop
   with (m . h) = (cons dirvish-mode-line-format dirvish-header-line-format)
   with (ml . mr) = (cons (plist-get m :left) (plist-get m :right))
   with (hl . hr) = (cons (plist-get h :left) (plist-get h :right))
   with attrs = (append '(hl-line symlink-target) dirvish-attributes)
   with feat-reqs = (append dirvish-preview-dispatchers attrs ml mr hl hr)
   when feat-reqs do (require 'dirvish-widgets)
   for (lib . feat) in dirvish--libraries do
   (when (cl-intersection feat feat-reqs) (require lib))
   finally (setf (dv-mode-line dv) (dirvish--mode-line-composer ml mr)
                 (dv-header-line dv) (dirvish--mode-line-composer hl hr t)
                 (dv-preview-dispatchers dv) (dirvish--preview-dps-validate)
                 (dv-attributes dv) (dirvish--attrs-expand attrs))))

(defun dirvish--only-index ()
  "If `dired-kill-when-opening-new-dired-buffer', only keep session index."
  (when-let* (((default-value 'dired-kill-when-opening-new-dired-buffer))
              (dv (dirvish-curr)) (index-buf (cdr (dv-index dv))))
    (cl-loop for (_d . b) in (dv-roots dv)
             unless (eq index-buf b) do (kill-buffer b))))

;;;; Advices

(defun dirvish-up-dir-a (fn args)
  "Ensure FN and ARGS applied with window undedicated."
  (dirvish-save-dedication (apply fn args))
  (dirvish--only-index))

(cl-defun dirvish-find-entry-a (&optional entry)
  "Find ENTRY in current dirvish session.
ENTRY can be a filename or a string with format of
`dirvish-fd-bufname' used to query or create a `fd' result
buffer, it defaults to filename under the cursor when it is nil."
  (let* ((entry (or entry (dired-get-filename nil t)))
         (buf (cond ((string-prefix-p "🔍" entry)
                     (dirvish-fd-find entry))
                    ((file-directory-p entry)
                     (dired-noselect entry))
                    ((string-suffix-p "/" entry)
                     (user-error (concat entry " is not a directory")))))
         (dv (dirvish-curr)) process-connection-type file)
    (when buf (dirvish-save-dedication (switch-to-buffer buf))
          (cl-return-from dirvish-find-entry-a (dirvish--only-index)))
    (setq file (expand-file-name entry))
    (cl-loop with e = (downcase (or (file-name-extension entry) ""))
             for (es . (c . a)) in dirvish-open-with-programs
             when (and (member e es) (executable-find c)) do
             (cl-return-from dirvish-find-entry-a
               (let ((a (cl-substitute file "%f" a :test #'string=)))
                 (apply #'start-process "" nil "nohup" (append (list c) a)))))
    (unless dv (cl-return-from dirvish-find-entry-a (find-file file)))
    (if-let* ((fn (dv-open-file-fn dv))) (funcall fn)
      (dirvish--clear-session dv))
    (find-file file)))

(defun dirvish-insert-subdir-a (dirname &rest _)
  "Setup newly inserted subdir DIRNAME for this Dirvish buffer."
  (dirvish--hide-dired-header)
  (dirvish--dir-data-async dirname (current-buffer) t))

(defun dirvish-wdired-enter-a (&rest _)
  "Advice for `wdired-change-to-wdired-mode'."
  (let (dirvish-hide-cursor) (dirvish--maybe-toggle-cursor 'hollow))
  (dirvish--render-attrs 'clear))

(defun dirvish-dired-noselect-a (fn dir-or-list &optional flags)
  "Return buffer for DIR-OR-LIST with FLAGS, FN is `dired-noselect'."
  (let* ((dir (if (consp dir-or-list) (car dir-or-list) dir-or-list))
         (key (file-name-as-directory (expand-file-name dir)))
         (reuse? (or (dirvish-curr) (dirvish--get-session)))
         (dv (or reuse? (dirvish--new)))
         (bname buffer-file-name)
         (remote (file-remote-p dir))
         (flags (or flags (dv-ls-switches dv)))
         (buffer (alist-get key (dv-roots dv) nil nil #'equal))
         (new-buffer-p (null buffer)))
    (when reuse? (setf (dv-reuse dv) t))
    (when new-buffer-p
      (if (not remote)
          (let ((dired-buffers nil)) ; disable reuse from dired
            (setq buffer (apply fn (list dir-or-list flags))))
        (require 'dirvish-tramp)
        (setq buffer (dirvish-tramp-noselect fn dir-or-list flags remote)))
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
      (dirvish-prop :dv (dv-id dv))
      (dirvish-prop :gui (display-graphic-p))
      (dirvish-prop :remote remote)
      (dirvish-prop :root key)
      (dirvish-prop :preview-dps
        (if remote '(dirvish-tramp-dp) (dv-preview-dispatchers dv)))
      (dirvish-prop :attrs (dv-attributes dv))
      (cl-loop for (k v) on dirvish-scopes by 'cddr
               do (dirvish-prop k (and (functionp v) (funcall v))))
      (when bname (dired-goto-file bname))
      (setf (dv-index dv) (cons key buffer))
      (run-hook-with-args 'dirvish-find-entry-hook key buffer)
      buffer)))

;;;; Hooks

(defun dirvish-apply-ansicolor-h (_win pos)
  "Update dirvish ansicolor in preview window from POS."
  (ansi-color-apply-on-region
   (goto-char pos) (progn (forward-line (frame-height)) (point))))

(defun dirvish-update-body-h ()
  "Update UI of current Dirvish."
  (when-let* ((dv (dirvish-curr)) ((null (derived-mode-p 'wdired-mode))))
    (cond ((not (dirvish--apply-hiding-p dirvish-hide-cursor)))
          ((eobp) (forward-line -1))
          ((cdr dired-subdir-alist))
          ((and (bobp) dirvish-use-header-line)
           (goto-char (dirvish-prop :content-begin))))
    (when (dirvish--apply-hiding-p dirvish-hide-cursor)
      (dired-move-to-filename))
    (dirvish--render-attrs)
    (when-let* ((filename (dired-get-filename nil t)))
      (dirvish-prop :index filename)
      (let ((h-buf (dirvish--util-buffer 'header dv t))
            (f-buf (dirvish--util-buffer 'footer dv t)))
        (dirvish-debounce nil
          (when (dv-curr-layout dv)
            (when (buffer-live-p f-buf)
              (with-current-buffer f-buf (force-mode-line-update)))
            (when (buffer-live-p h-buf)
              (with-current-buffer h-buf (force-mode-line-update)))
            (unless (dirvish--get-session 'type 'peek) ; don't grab focus
              (dirvish--preview-update dv filename))))))))

(defun dirvish-insert-entry-h (entry buffer)
  "Add ENTRY or BUFFER name to `dirvish--history'."
  (let ((entry (if (string-prefix-p "🔍" entry)
                   (buffer-name buffer) entry)))
    (setq dirvish--history (seq-take (push entry dirvish--history) 200))))

(defun dirvish-kill-buffer-h ()
  "Remove buffer from session's buffer list."
  (when-let* ((dv (dirvish-curr)) (buf (current-buffer)))
    (setf (dv-roots dv) (cl-remove-if (lambda (i) (eq (cdr i) buf)) (dv-roots dv)))
    (if (dv-roots dv) ; it might be killed by user in a fullframe session
        (when-let* (((eq (cdr (dv-index dv)) buf))
                    ((dv-curr-layout dv))
                    (win (dv-root-window dv))
                    ((window-live-p win)))
          (setf (dv-index dv) (car (dv-roots dv)))
          (with-selected-window win ; we have to prevend this window get deleted
            (dirvish-save-dedication (switch-to-buffer (cdr (dv-index dv))))))
      (when-let* ((layout (dv-curr-layout dv))
                  (wconf (dv-winconf dv))
                  ((eq buf (window-buffer (selected-window)))))
        (set-window-configuration wconf))
      (remhash (dv-id dv) dirvish--session-hash)
      (cl-loop for b in (buffer-list) for bn = (buffer-name b) when
               (string-match-p (format " ?\\*Dirvish-.*-%s\\*" (dv-id dv)) bn)
               do (dirvish--kill-buffer b)))))

(defun dirvish-winbuf-change-h (window)
  "Rebuild layout once buffer in WINDOW changed."
  (when-let* ((dv (with-selected-window window (dirvish-curr)))
              (dir (car (dv-index dv))) (buf (cdr (dv-index dv)))
              (winconf t) (layout t)
              (old-tab (with-selected-window window (dirvish-prop :tab)))
              (old-frame (with-selected-window window (dirvish-prop :frame)))
              (sc (cl-loop for (k v) on dirvish-scopes by 'cddr
                           append (list k (and (functionp v) (funcall v)))))
              (frame t) (tab t))
    (setq winconf (dv-winconf dv) layout (dv-curr-layout dv)
          frame (plist-get sc :frame) tab (plist-get sc :tab))
    (when (and (dv-reuse dv) (not (equal old-frame frame)))
      (with-selected-window (frame-selected-window old-frame)
        (when (dirvish-curr) (let (dirvish-reuse-session) (dirvish-quit)))
        (setq dv (dirvish--new))))
    (when (and (dv-reuse dv) (not (equal old-tab tab)))
      ;; TODO: maybe clear dirvish sessions in all tabs except the current TAB?
      (setq dv (dirvish--new)))
    (cond
     ;; by `*-other-tab|frame'
     ((or (null (equal old-frame frame)) (null (equal old-tab tab)))
      (with-selected-window (dirvish--create-root-window dv)
        (dirvish-save-dedication
         (switch-to-buffer (get-buffer-create "*scratch*")))
        (setf (dv-winconf dv) (current-window-configuration))
        (dirvish-save-dedication (switch-to-buffer (dired-noselect dir)))
        (cl-loop for (k v) on sc by 'cddr do (dirvish-prop k v))
        (dirvish--build-layout dv)))
     (t (with-selected-window window (dirvish--build-layout dv))))))

;;;; Preview

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
             `(info . ,(format "File [ %s ] contains very long lines" name)))
            (t (with-current-buffer buf (font-lock-mode 1) `(buffer . ,buf)))))))

(dirvish-define-preview disable (file ext)
  "Disable preview in some cases."
  (cond
   ((not (file-exists-p file))
    `(info . ,(format "[ %s ] does not exist" file)))
   ((not (file-readable-p file))
    `(info . ,(format "[ %s ] is not readable" file)))
   ((member ext dirvish-preview-disabled-exts)
    `(info . ,(format "Preview for filetype [ %s ] has been disabled" ext)))))

(dirvish-define-preview dired (file)
  "Preview dispatcher for directory FILE."
  (when (file-directory-p file)
    `(dired . (let ,(mapcar (lambda (env) `(,(car env) ,(cdr env)))
                            (remove (cons 'inhibit-message t)
                                    dirvish-preview-environment))
                (setq insert-directory-program ,insert-directory-program)
                (setq dired-listing-switches ,dired-listing-switches)
                (setq dired-omit-verbose ,(bound-and-true-p dired-omit-verbose))
                (setq dired-omit-files ,(bound-and-true-p dired-omit-files))
                (with-current-buffer (dired-noselect ,file)
                  ,(and dirvish-preview-dired-sync-omit
                        (bound-and-true-p dired-omit-mode)
                        `(dired-omit-mode))
                  (message "\n%s" (buffer-string)))))))

(dirvish-define-preview fallback (file ext)
  "Fallback preview dispatcher for FILE."
  (let* ((attrs (ignore-errors (file-attributes file)))
         (size (file-attribute-size attrs)))
    (cond ((not attrs)
           `(info . ,(format "Can not get attributes of [ %s ]." file)))
          ((not size)
           `(info . ,(format "Can not get file size of [ %s ]." file)))
          ((> size (or large-file-warning-threshold 10000000))
           `(info . ,(format "File [ %s ] is too big for literal preview." file)))
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
      (insert "\n\n  " (cdr recipe)) buf)))

(cl-defmethod dirvish-preview-dispatch ((recipe (head buffer)) dv)
  "Use payload of RECIPE as preview buffer of DV directly."
  (let ((p-buf (dirvish--util-buffer 'preview dv nil t)))
    (with-current-buffer p-buf (erase-buffer) (remove-overlays) (cdr recipe))))

(defun dirvish-shell-preview-proc-s (proc _exitcode)
  "A sentinel for dirvish preview process.
When PROC finishes, fill preview buffer with process result."
  (when-let* ((dv (dirvish-curr)))
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
  (let ((proc (get-buffer-process (get-buffer " *dirvish-sh*")))
        (buf (dirvish--util-buffer 'preview dv nil t)))
    (when proc (delete-process proc))
    (dirvish--make-proc
     (cdr recipe) 'dirvish-shell-preview-proc-s " *dirvish-sh*"
     'cmd-info (car recipe))
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
              (fns (with-current-buffer (window-buffer (dv-root-window dv))
                     (dirvish-prop :preview-dps)))
              (buf (cl-loop for fn in fns
                            for rcp = (funcall fn index ext window dv) thereis
                            (and rcp (dirvish-preview-dispatch rcp dv)))))
    (setq-local other-window-scroll-buffer buf)
    (set-window-buffer window buf)
    (unless (memq buf orig-bufs) (push buf (dv-preview-buffers dv)))))

;;;; Attributes

(defun dirvish--render-attrs-1 (height width pos remote fns ov align-to no-hl)
  "HEIGHT WIDTH POS REMOTE FNS OV ALIGN-TO NO-HL."
  (forward-line (- 0 height))
  (cl-dotimes (_ (* 2 height))
    (when (eobp) (cl-return))
    (let ((f-beg (dired-move-to-filename))
          (f-end (dired-move-to-end-of-filename t))
          (l-beg (line-beginning-position)) (l-end (line-end-position))
          (f-wid 0) f-str f-name f-attrs f-type hl-face left right)
      (setq hl-face (and (eq (or f-beg l-beg) pos) no-hl 'dirvish-hl-line))
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
           with no-hl = (dirvish--apply-hiding-p dirvish-hide-cursor)
           with remain = (- (window-width) (if gui 1 2))
           for (_ width pred render ov) in (dirvish-prop :attrs)
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
                                          remote fns ov (if gui 0 2) no-hl))))))

(dirvish-define-attribute hl-line
  "Highlight current line.
This attribute is disabled when cursor is visible."
  (when hl-face
    (let ((ov (make-overlay l-beg (1+ l-end))))
      (overlay-put ov 'face hl-face) `(ov . ,ov))))

(dirvish-define-attribute symlink-target
  "Hide symlink target."
  :when (or (derived-mode-p 'dirvish-directory-view-mode)
            (and dired-hide-details-mode
                 (default-value 'dired-hide-details-hide-symlink-targets)))
  (when (< (+ f-end 4) l-end)
    (let ((ov (make-overlay f-end l-end)))
      (overlay-put ov 'invisible t) `(ov . ,ov))))

;;;; Mode Line | Header Line

(defun dirvish--mode-line-composer (left right &optional header)
  "Compose `mode-line-format' from LEFT and RIGHT segments.
If HEADER, the format is used for `header-line-format'."
  `((:eval
     (let* ((dv (dirvish-curr))
            (fullframe-p (and dv (dv-curr-layout dv)))
            (buf (if fullframe-p (cdr (dv-index dv)) (current-buffer)))
            (expand
             (lambda (segs)
               (cl-loop for s in segs collect
                        (if (stringp s) s
                          `(:eval (,(intern (format "dirvish-%s-ml" s))))))))
            (face ',(if header 'header-line 'mode-line-inactive))
            (default (face-attribute 'default :height))
            (ml-height (face-attribute face :height))
            (scale (cond ((floatp ml-height) ml-height)
                         ((integerp ml-height) (/ (float ml-height) default))
                         (t 1)))
            (win-width (floor (/ (window-width) scale)))
            (str-l (if dv " DIRVISH: context buffer is a killed buffer"
                     " DIRVISH: failed to get current session"))
            (str-r (propertize "WARNING " 'face 'dired-warning))
            (len-r 8))
       (when (buffer-live-p buf)
         (setq str-l (format-mode-line (funcall expand ',left) nil nil buf))
         (setq str-r (format-mode-line (funcall expand ',right) nil nil buf))
         (setq len-r (string-width str-r)))
       (concat
        (dirvish--mode-line-bar-img fullframe-p ,header)
        (if (< (+ (string-width str-l) len-r) win-width)
            str-l
          (let ((trim (1- (- win-width len-r))))
            (if (>= trim 0)
                (substring str-l 0 (min trim (1- (length str-l))))
              "")))
        (propertize
         " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                            ,(ceiling (* scale len-r))))))
        str-r)))))

;; Thanks to `doom-modeline'.
(defun dirvish--mode-line-bar-img (fullframe-p header)
  "Create a bar image with height of `dirvish-mode-line-height'.
If FULLFRAME-P, use the `cdr' of the value as height, otherwise
use `car'.  If HEADER, use `dirvish-header-line-height' instead."
  (when (and (display-graphic-p) (image-type-available-p 'pbm))
    (let* ((hv (if header dirvish-header-line-height dirvish-mode-line-height))
           (ht (cond ((numberp hv) hv) (fullframe-p (cdr hv)) (t (car hv)))))
      (propertize
       " " 'display
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" 2 ht) (make-string (* 2 ht) ?1) "\n")
          'pbm t :foreground "None" :ascent 'center))))))

(defun dirvish--setup-mode-line (dv)
  "Setup the mode/header line for dirvish DV."
  (let* ((idx-buf (cdr (dv-index dv)))
         (hl (or (dirvish-prop :cus-header) (dv-header-line dv)))
         (ml (dv-mode-line dv))
         (fullframe-p (dv-curr-layout dv)))
    (cond ; setup `header-line-format'
     ((and fullframe-p (not dirvish-use-header-line)))
     (fullframe-p
      (with-current-buffer idx-buf (setq header-line-format nil))
      (with-current-buffer (dirvish--util-buffer 'header dv)
        (setq header-line-format hl)))
     (dirvish-use-header-line
      (with-current-buffer idx-buf (setq header-line-format hl))))
    (cond ; setup `mode-line-format'
     ((and fullframe-p (not dirvish-use-mode-line)))
     (fullframe-p
      (with-current-buffer idx-buf (setq mode-line-format nil))
      (with-current-buffer (dirvish--util-buffer 'footer dv)
        (setq mode-line-format ml)))
     (dirvish-use-mode-line
      (with-current-buffer idx-buf (setq mode-line-format ml))))))

(defun dirvish--apply-hiding-p (ctx)
  "Return t when it should hide cursor/details within context CTX."
  (cond ((booleanp ctx) ctx)
        ((dirvish-prop :fd-switches)
         (memq 'dirvish-fd ctx))
        ((and (dirvish-curr) (dv-curr-layout (dirvish-curr)))
         (memq 'dirvish ctx))
        ((and (dirvish-curr) (eq (dv-type (dirvish-curr)) 'side))
         (memq 'dirvish-side ctx))
        (t (memq 'dired ctx))))

(defun dirvish--maybe-toggle-cursor (&optional cursor)
  "Toggle cursor's invisibility according to context.
Optionally, use CURSOR as the enabled cursor type."
  (if (dirvish--apply-hiding-p dirvish-hide-cursor)
      (prog1 (setq-local cursor-type nil)
        (cond ((bound-and-true-p evil-local-mode)
               (setq-local evil-normal-state-cursor '(bar . 0)))
              ((bound-and-true-p meow-motion-mode)
               (setq-local meow-cursor-type-motion nil))))
    (setq-local cursor-type (or cursor '(box . 4)))
    (cond ((bound-and-true-p evil-local-mode)
           (setq-local evil-normal-state-cursor (or cursor '(box . 4))))
          ((bound-and-true-p meow-motion-mode)
           (setq-local meow-cursor-type-motion (or cursor '(box . 4)))))))

(defun dirvish--maybe-toggle-details ()
  "Toggle `dired-hide-details-mode' according to context."
  (if (dirvish--apply-hiding-p dirvish-hide-details)
      (dired-hide-details-mode 1)
    (dired-hide-details-mode -1)))

;;;; Layout Builder

(defun dirvish-revert (&optional ignore-auto _noconfirm)
  "Reread the Dirvish buffer.
When IGNORE-AUTO, refresh file attributes as well.
Dirvish sets `revert-buffer-function' to this function."
  (dirvish-prop :old-index (dired-get-filename nil t))
  (dolist (keyword dirvish--reset-keywords) (dirvish-prop keyword nil))
  (dired-revert)
  (dirvish--hide-dired-header)
  (when ignore-auto ; meaning it is called interactively from user
    (setq-local dirvish--attrs-hash (make-hash-table :test #'equal))
    (dirvish--dir-data-async default-directory (current-buffer)))
  (run-hooks 'dirvish-after-revert-hook))

(defun dirvish-init-dired-buffer ()
  "Initialize a Dired buffer for Dirvish."
  (use-local-map dirvish-mode-map)
  (dirvish--hide-dired-header)
  (dirvish--maybe-toggle-cursor 'box) ; restore from `wdired'
  (setq-local dirvish--attrs-hash
              (or dirvish--attrs-hash (make-hash-table :test #'equal))
              revert-buffer-function #'dirvish-revert
              tab-bar-new-tab-choice "*scratch*"
              dired-hide-details-hide-symlink-targets nil
              dired-kill-when-opening-new-dired-buffer nil)
  (add-hook 'window-selection-change-functions #'dirvish--change-selected nil t)
  (add-hook 'window-configuration-change-hook #'dirvish-update-body-h nil t)
  (add-hook 'window-buffer-change-functions #'dirvish-winbuf-change-h nil t)
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
      (dirvish-prop :dv (dv-id dv))
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
      (dirvish--maybe-toggle-cursor '(box . 0)) ; always hide cursor in parents
      (dirvish-prop :attrs (dirvish--attrs-expand attrs))
      (setq-local dirvish--attrs-hash (make-hash-table :test #'equal))
      (dirvish--render-attrs) buf)))

(defun dirvish--create-parent-windows (dv)
  "Create all dirvish parent windows for DV."
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish--get-parent-path current))
         (parent-dirs ())
         (depth (or (car (dv-curr-layout dv)) 0))
         (i 0))
    (while (and (< i depth) (not (string= current parent)))
      (cl-incf i)
      (push (cons current parent) parent-dirs)
      (setq current (dirvish--get-parent-path current))
      (setq parent (dirvish--get-parent-path parent)))
    (when (> depth 0)
      (cl-loop with layout = (dv-curr-layout dv)
               with parent-width = (nth 1 layout)
               with remain = (- 1 (nth 2 layout) parent-width)
               with width = (min (/ remain depth) parent-width)
               for level from 1 for (current . parent) in parent-dirs
               for args = `((side . left) (inhibit-same-window . t)
                            (window-width . ,width)
                            (window-parameters . ((no-other-window . t))))
               for b = (dirvish--create-parent-buffer dv parent current level)
               for w = (display-buffer b `(dirvish--display-buffer . ,args)) do
               (with-selected-window w (set-window-dedicated-p w t))))))

(defun dirvish--init-util-buffers (dv)
  "Initialize util buffers for DV."
  (with-current-buffer (dirvish--util-buffer 'preview dv nil t)
    (fundamental-mode) (setq mode-line-format nil header-line-format nil)
    (add-hook 'window-scroll-functions #'dirvish-apply-ansicolor-h nil t))
  (with-current-buffer (dirvish--util-buffer 'header dv)
    (dirvish-prop :dv (dv-id dv))
    (setq-local face-remapping-alist '((header-line-inactive header-line)))
    (setq cursor-type nil window-size-fixed 'height
          mode-line-format nil header-line-format nil))
  (with-current-buffer (dirvish--util-buffer 'footer dv)
    (dirvish-prop :dv (dv-id dv))
    (setq-local face-remapping-alist '((mode-line-inactive mode-line)))
    (setq cursor-type nil window-size-fixed 'height
          mode-line-format nil header-line-format nil)))

(defun dirvish--dir-data-async (dir buffer &optional inhibit-setup)
  "Asynchronously fetch metadata for DIR, stored locally in BUFFER.
INHIBIT-SETUP is passed to `dirvish-data-for-dir'."
  (dirvish--make-proc
   `(prin1
     (let* ((hs (make-hash-table))
            (remote? (file-remote-p ,dir))
            (bk (unless remote? (vc-responsible-backend ,dir t))))
       (dolist (file (unless remote? (directory-files ,dir t nil t)))
         (let* ((attrs (file-attributes file)) (tp (nth 0 attrs)))
           (cond ((eq t tp) (setq tp '(dir . nil)))
                 (tp (setq tp `(,(if (file-directory-p tp) 'dir 'file) . ,tp)))
                 (t (setq tp '(file . nil))))
           (puthash (secure-hash 'md5 file) `(:builtin ,attrs :type ,tp) hs)))
       (cons bk hs)))
   (lambda (p _)
     (pcase-let ((`(,buf . ,inhibit-setup) (process-get p 'meta))
                 (`(,vc . ,data)
                  (with-current-buffer (process-buffer p)
                    (read (buffer-string)))))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (maphash (lambda (k v) (puthash k v dirvish--attrs-hash)) data)
           (dirvish-prop :vc-backend (or vc 0)) ; for &context compat
           (dirvish-data-for-dir dir buf inhibit-setup))))
     (delete-process p)
     (dirvish--kill-buffer (process-buffer p)))
   nil 'meta (cons buffer inhibit-setup)))

(cl-defgeneric dirvish-data-for-dir (dir buffer inhibit-setup)
  "Fetch data for DIR in BUFFER.
It is called when DIR is in localhost and is not being
version-controlled.  Run `dirvish-setup-hook' after data parsing unless
INHIBIT-SETUP is non-nil."
  (ignore dir buffer)
  (unless inhibit-setup (run-hooks 'dirvish-setup-hook))
  (when-let* ((win (get-buffer-window buffer))
              ((window-live-p win)))
    (with-selected-window win (dirvish-update-body-h))))

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

(defun dirvish--build-layout (dv)
  "Build layout for Dirvish session DV."
  (setf (dv-index dv) (cons (dirvish-prop :root) (current-buffer)))
  (setf (dv-winconf dv) (or (dv-winconf dv) (current-window-configuration)))
  (let* ((layout (dv-curr-layout dv))
         (w-args `((preview (side . right) (window-width . ,(nth 2 layout)))
                   (header (side . above) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))
                   (footer (side . below) (window-height . -2)
                           (window-parameters . ((no-other-window . t))))))
         (w-order (and layout (dirvish--window-split-order))) util-windows)
    (dirvish--init-util-buffers dv)
    (dirvish--setup-mode-line dv)
    (when w-order (let ((ignore-window-parameters t)) (delete-other-windows)))
    (dirvish-prop :fringe nil)
    (dirvish--change-selected)
    (when-let* ((fixed (dv-size-fixed dv))) (setq window-size-fixed fixed))
    (when (or (dv-curr-layout dv) (dv-dedicated dv))
      (set-window-dedicated-p nil t))
    (dolist (pane w-order)
      (let* ((buf (dirvish--util-buffer pane dv nil (eq pane 'preview)))
             (args (alist-get pane w-args))
             (win (display-buffer buf `(dirvish--display-buffer . ,args))))
        (cond ((eq pane 'preview) (setf (dv-preview-window dv) win))
              (t (set-window-dedicated-p win t) (push win util-windows)))
        (set-window-buffer win buf)))
    (dirvish--create-parent-windows dv)
    (when (and (display-graphic-p) (> emacs-major-version 28))
      (let ((window-safe-min-height 0) (window-resize-pixelwise t))
        (dolist (win util-windows) (fit-window-to-buffer win 2 1))))
    (unless (dirvish-prop :cached)
      (dirvish--dir-data-async default-directory (current-buffer))
      (dirvish-prop :cached t))
    (dirvish--maybe-toggle-cursor)
    (dirvish--maybe-toggle-details)))

(defun dirvish--reuse-or-create (path &optional dwim)
  "Find PATH in dirvish, check `one-window-p' for DWIM."
  (let* ((dir (or path default-directory))
         (vis? (cl-loop for w in (window-list)
                        for b = (window-buffer w)
                        for dv = (with-current-buffer b (dirvish-curr))
                        thereis (and dv (eq 'default (dv-type dv)) dv)))
         (reuse? (unless vis? (dirvish--get-session 'type 'default))))
    (cond (vis? (when (and (null dwim) dirvish-default-layout
                           (not (dv-curr-layout vis?)))
                  (unless (dirvish-curr) (select-window (dv-root-window vis?)))
                  (dirvish-layout-toggle))
                (dirvish-find-entry-a dir))
          (reuse?
           (with-selected-window (dirvish--create-root-window reuse?)
             (setf (dv-curr-layout reuse?)
                   (or (dv-curr-layout reuse?) dirvish-default-layout))
             (and dwim (not (one-window-p)) (setf (dv-curr-layout reuse?) nil))
             (if (eq dirvish-reuse-session 'resume) (dirvish--build-layout reuse?)
               (dirvish-find-entry-a dir))))
          (t (dirvish--new
              :curr-layout (if dwim (and (one-window-p) dirvish-default-layout)
                             dirvish-default-layout))
             (dirvish-find-entry-a dir)))))

(define-derived-mode dirvish-directory-view-mode
  fundamental-mode "Dirvish-directory-view"
  "Major mode for dirvish parent buffers."
  :group 'dirvish :interactive nil)

;;;; Commands

(defun dirvish-layout-toggle ()
  "Toggle layout of current Dirvish session.
A session with layout means it has a companion preview window and
possibly one or more parent windows."
  (interactive)
  (let* ((dv (or (dirvish-curr) (user-error "Not a dirvish buffer")))
         (old-layout (dv-curr-layout dv))
         (new-layout (unless old-layout (dv-ff-layout dv)))
         (buf (current-buffer)))
    (if old-layout (set-window-configuration (dv-winconf dv))
      (with-selected-window (dv-root-window dv) (quit-window)))
    (setf (dv-curr-layout dv) new-layout)
    (with-selected-window (dirvish--create-root-window dv)
      (dirvish-save-dedication (switch-to-buffer buf))
      (dirvish--build-layout dv))))

(defun dirvish-quit ()
  "Quit current Dirvish session.
If the session is a full-framed one, the window layout is restored.  If
`dirvish-reuse-session' is nil, all Dired buffers in the session are
killed, otherwise only the invisible Dired buffers within the session
are killed and the Dired buffer(s) in the selected window are buried."
  (interactive)
  (let ((dv (dirvish-curr)) (ct 0) (lst (window-list))
        (win (selected-window)) (frame (selected-frame)))
    (dirvish--clear-session dv t)
    (while (and (dirvish-curr) (eq (selected-window) win)
                (<= (cl-incf ct) (length lst)))
      (quit-window))
    (unless (eq (selected-frame) frame) (delete-frame frame))))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Let Dirvish take over Dired globally."
  :group 'dirvish :global t
  (let ((ads '((dired-find-file dirvish-find-entry-a :override)
               (dired-up-directory dirvish-up-dir-a :around)
               (dired-noselect dirvish-dired-noselect-a :around)
               (dired-insert-subdir dirvish-insert-subdir-a :after)
               (wdired-change-to-wdired-mode dirvish-wdired-enter-a :after)
               (wdired-change-to-dired-mode dirvish-init-dired-buffer :after))))
    (if dirvish-override-dired-mode
        (pcase-dolist (`(,sym ,fn ,how) ads) (advice-add sym how fn))
      (pcase-dolist (`(,sym ,fn) ads) (advice-remove sym fn)))))

;;;###autoload
(defun dirvish (&optional path)
  "Open PATH in a fullframe Dirvish session.
Prompt for PATH if called with \\[universal-arguments], otherwise PATH
defaults to `default-directory'."
  (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
  (dirvish--reuse-or-create path))

;;;###autoload
(defun dirvish-dwim (&optional path)
  "Open PATH in a fullframe session if selected window is the only window.
Prompt for PATH if called with \\[universal-arguments], otherwise PATH
defaults to `default-directory'.  If there are other windows exist in the
selected frame, the session occupies only the selected window."
  (interactive (list (and current-prefix-arg (read-directory-name "Dirvish: "))))
  (dirvish--reuse-or-create path 'dwim))

(provide 'dirvish)
;;; dirvish.el ends here
