;;; lf.el --- A better dired interface -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Package-Version: 0.5
;; Keywords: files, convenience, dired
;; Homepage: https://github.com/alexluigit/lf.el
;; SPDX-License-Identifier: MIT
;; Package-Requires: ((emacs "27.1") (posframe "1.0.2"))

;;; Inspired by work from
;; ranger.el - Author: Rich Alesi

;;; Code:

(declare-function format-spec "format-spec")
(require 'ring)
(require 'transient)
(require 'posframe)
(require 'mailcap)
(require 'dired-x)
(require 'all-the-icons)
(require 'ansi-color)
(eval-when-compile (require 'subr-x))

(defgroup lf nil
  "Modify dired to act like Lf."
  :group 'dired)

;;; Variables

;;;; User facing options

(defcustom lf-show-hidden 'all
  "Determine hidden method in lf."
  :group 'lf
  :type '(radio (const :tag "Show All Files" :value 'all)
                (const :tag "Hide Common Files" :value 'lf)
                (const :tag "Hide All Dotfiles" :value 'dot)))

(defcustom lf-hidden-regexp
  '("^\\.\\(git\\|hg\\|svn\\)$"
    "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
    "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
    "^\\.\\(sync\\|export\\|attach\\)$"
    "~$" "^#.*#$")
  "Regexp of custom filetypes to omit in lf."
  :group 'lf :type 'list)

(defcustom lf-preview-cmd-alist
  '((nil ("iso" "bin" "exe") ("*Preview-Disable*"))
    (nil ("zip")             ("zipinfo" "%i"))
    (nil ("zst" "tar")       ("tar" "-tvf" "%i"))
    (nil "audio"             ("mediainfo" "%i"))
    (t   "image"             ("convert" "-resize" "%s" "%i" "%T"))
    (t   "video"             ("ffmpegthumbnailer" "-i" "%i" "-o" "%T" "-s 0"))
    (t   ("rmvb" "f4v" "ts") ("ffmpegthumbnailer" "-i" "%i" "-o" "%T" "-s 0"))
    (t   ("epub")            ("epub-thumbnailer" "%i" "%T" "1024"))
    (t   ("pdf")             ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" "%i" "%t")))
  "doc"
  :group 'lf :type '(alist :value-type (boolean (choice list string) list)))

(defcustom lf-history-length 30
  "Length of history lf will track."
  :group 'lf :type 'integer)

(defcustom lf-cache-dir
  (concat (or (getenv "XDG_CACHE_HOME") (concat (getenv "HOME") "/.cache")) "/lf/")
  "Preview / thumbnail cache directory for lf."
  :group 'lf :type 'string)

(defcustom lf-depth 1
  "Number of directories up to traverse."
  :group 'lf :type 'integer)

(defcustom lf-enable-preview t
  "When not-nil preview the selected file."
  :group 'lf :type 'boolean)

(defcustom lf-preview-binary nil
  "When not-nil preview binary files."
  :group 'lf :type 'boolean)

(defcustom lf-width-parents 0.12
  "Fraction of frame width taken by parent windows."
  :group 'lf :type 'float)

(defcustom lf-max-parent-width 0.25
  "The max width allocated to showing parent windows."
  :group 'lf :type 'float)

(defcustom lf-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'lf :type 'float)

(defcustom lf-file-line-padding 0.08
  "doc"
  :group 'lf :type 'float)

(defcustom lf-header-string-fn 'lf--header-string
  "Function used to output a string that will show up as header."
  :group 'lf :type 'function)

(defcustom lf-header-position '(0 . 20)
  "Position of header line."
  :group 'lf :type '(choice (number cons function)))

(defcustom lf-footer-format "%S  %f  %d  %p%w%t %i"
  "Format for footer display. "
  :group 'lf :type 'string)

(defcustom lf-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR) where DISK is path to a disk and
TRASH-DIR is path to trash-dir in that disk."
  :group 'lf :type 'alist)

(defcustom lf-show-icons t
  "When not-nil show file / dir icon."
  :group 'lf :type 'boolean)

(defcustom lf-icons-monochrome t
  "doc"
  :group 'lf :type 'boolean)

(defcustom lf-icons-v-offset 0.01
  "doc"
  :group 'lf :type 'float)

;;;; Faces

(defgroup lf-faces nil
  "Faces used by Lf."
  :group 'lf :group 'faces)

(defface lf-line-face
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Face used for `lf-update--line'."
  :group 'lf-faces)

;;;; Internal variables

(defvar lf-yank-marker ?Y
  "Character used to flag files for yank.")

(defvar lf-width-img nil
  "Calculated preview window width. Used for image preview.")

(defvar lf-width-header (frame-width)
  "Calculated header frame width. Default to frame width when disable preview.")

(defvar lf-header-scale 1.25
  "Height of header line.")

(defvar lf-history-ring (make-ring lf-history-length)
  "History for `lf-find-file'.")

(defvar lf-pre-config-saved nil
  "Indicate if previous window config saved.")

(defvar lf-preview-delay 0.02
  "Time in seconds to delay running preview file functions.")

(defvar lf-footer-repeat 0.02
  "Time in seconds to repeat footer update.")

(defvar lf-sort-criteria ""
  "Default `ls' sorting switches.")

(defvar lf-window nil
  "Main lf window. Adjacent to preview window.")

(defvar lf-parent-windows ()
  "List of parent windows.")

(defvar lf-parent-buffers ()
  "List with buffers of parent buffers.")

(defvar lf-preview-buffers ()
  "List with buffers of previewed files.")

(defvar lf-frame-alist ()
  "List of frames using lf.")

(defvar lf-parent-win-hook '(lf-default-parent-config)
  "Hooks for setting lf parent windows.")

(defvar lf-preview-win-hook '(lf-default-preview-config)
  "Hooks for setting lf preview windows.")

(defvar lf-repeat-timers '()
  "Timers with repeat flag need to be clean when exit.")

(defvar lf-i/o-queue ()
  "doc")

(defvar lf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; file opening
    (define-key map "ws"               (lambda () (interactive) (lf-open-file 'vertical)))
    (define-key map "wv"               (lambda () (interactive) (lf-open-file 'horizontal)))
    (define-key map "wo"               (lambda () (interactive) (lf-open-file 'other)))
    (define-key map "we"               'lf-open-in-external-app)
    (define-key map "e"                'lf-find-file)
    (define-key map (kbd "<return>")   'lf-find-file)
    (define-key map (kbd "<mouse-1>")  'lf-find-file)
    ;; navigation
    (define-key map "G"                'lf-go-bottom)
    (define-key map "h"                'lf-up-directory)
    (define-key map "n"                'lf-next-file)
    (define-key map "p"                'lf-prev-file)
    (define-key map (kbd "C-d")        'lf-page-down)
    (define-key map (kbd "C-u")        'lf-page-up)
    (define-key map "g"                'lf-go)
    ;; copy and paste
    (define-key map "y"                'lf-flag-file-yank)
    (define-key map "kk"               'lf-paste)
    (define-key map "km"               'lf-move)
    (define-key map "kl"               'lf-paste-as-symlink)
    (define-key map "ks"               'lf-show-yank-contents)
    ;; settings
    (define-key map "s"                'lf-sort-criteria)
    (define-key map "z+"               'lf-more-parents)
    (define-key map "z-"               'lf-less-parents)
    (define-key map "zh"               'lf-toggle-dotfiles)
    (define-key map "zp"               'lf-toggle-preview)
    (define-key map "r"                'lf-refresh)
    (define-key map "q"                'lf-quit)
    ;; utils
    (define-key map "fn"               'lf-yank-filename)
    (define-key map "fp"               'lf-yank-filepath)
    (define-key map "ft"               'dired-show-file-type)
    (define-key map "v"                'set-mark-command)
    (define-key map (kbd "<tab>")      'lf-show-history)
    (define-key map "a"                'lf-rename-eol)
    (define-key map "i"                'wdired-change-to-wdired-mode)
    (define-key map "I"                'dired-insert-subdir)
    (define-key map "E"                'dired-create-empty-file)
    (define-key map "/"                'lf-search)
    (define-key map (kbd "C-s")        'lf-search-next)
    (define-key map (kbd "C-r")        'lf-search-previous)
    (define-key map [remap delete-window]
      (lambda () (interactive)
        (message "Press %s to quit" (propertize "q" 'face 'help-key-binding))))
    map)
  "Lf mode map.")

(defvar lf-override-dired-mode nil)

;;;; Buffer / Frame local variables

(defvar-local lf-child-entry nil
  "doc")

(defvar lf-index-path nil
  "Latest path in lf-window.")

(defvar lf-preview-window nil
  "Window contains file / directory preview.")

(defvar lf-preview-buffer nil
  "doc")

(defvar lf-header-buffer nil
  "Buffer for showing header line.")

(defvar lf-header--frame nil
  "Frame for showing header line.")

(defvar lf-cleanup-regex "*Lf \\(I/O\\|Preview\\|Header\\)*"
  "doc")

;;; Layout

;;;; Header frame

(defun lf-build--header-frame ()
  (let* ((buf (frame-parameter nil 'lf-header-buffer))
         (min-w (1+ (ceiling lf-width-header)))
         (f-props `(:min-height 2 :background-color "#565761"
                                :position ,lf-header-position :min-width ,min-w))
         (h-frame (frame-parameter nil 'lf-header--frame)))
    (if h-frame
        (posframe--set-frame-size h-frame 1 2 1 min-w)
      (set-frame-parameter nil 'lf-header--frame (apply #'posframe-show buf f-props)))))

;;;; Parent windows

(defun lf-build--parent-windows ()
  (let* ((current (expand-file-name default-directory))
         (parent (lf-get--parent current))
         (parent-dirs ()) (i 0))
    (when lf-child-entry (dired-goto-file lf-child-entry))
    (delete-other-windows)
    (setq lf-window (frame-selected-window))
    (add-to-list 'lf-parent-windows lf-window)
    (add-to-list 'lf-parent-buffers (current-buffer))
    (lf-mode)
    (while (and (< i lf-depth) (not (string= current parent)))
      (setq i (+ i 1))
      (push (cons current parent) parent-dirs)
      (setq current (lf-get--parent current))
      (setq parent (lf-get--parent parent)))
    (let ((width (min (/ lf-max-parent-width lf-depth) lf-width-parents)))
      (cl-dolist (parent-dir parent-dirs)
        (let* ((current (car parent-dir))
               (parent (cdr parent-dir))
               (win-alist `((side . left)
                            (inhibit-same-window . t)
                            (window-width . ,width)))
               (buffer (dired-noselect parent))
               (window (display-buffer buffer `(lf-display--buffer . ,win-alist))))
          (with-current-buffer buffer
            (when (and lf-show-icons lf-child-entry)
              (remove-overlays (1- (point)) (point) 'lf-icons t)
              (lf-render--icon (point)))
            (setq lf-child-entry current)
            (add-to-list 'lf-parent-buffers buffer)
            (add-to-list 'lf-parent-windows window))))
      (cl-dolist (buf lf-parent-buffers)
        (with-current-buffer buf
          (when lf-child-entry (dired-goto-file lf-child-entry))
          (when (dired-move-to-filename nil) (lf-update--line))
          (run-hooks 'lf-parent-win-hook))))))

(defun lf-default-parent-config ()
  "Default parent windows settings."
  (setq mode-line-format nil)
  (setq truncate-lines t)
  (setq cursor-type nil)
  (dired-hide-details-mode t)
  (display-line-numbers-mode -1))

;;;; Preview window

(defun lf-build--preview-window ()
  (when lf-enable-preview
    (let* ((inhibit-modification-hooks t)
           (win-alist `((side . right) (window-width . ,lf-width-preview)))
           (buf (frame-parameter nil 'lf-preview-buffer))
           (new-window (display-buffer buf `(lf-display--buffer . ,win-alist))))
      (setq lf-width-img (window-width new-window t))
      (setq lf-width-header (* (frame-width) (- 1 lf-width-preview)))
      (set-frame-parameter nil 'lf-preview-window new-window))))

(defun lf-default-preview-config ()
  "Default lf preview window config."
  (setq cursor-type nil)
  (setq truncate-lines t))

;;; Update

;;;; Preview

(cl-defun lf-get--preview-create (entry &optional cache cmd args)
  "Get corresponding preview buffer."
  (let ((buf (frame-parameter nil 'lf-preview-buffer))
        (process-connection-type nil)
        (size (number-to-string lf-width-img)))
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (unless cmd (insert entry)
              (cl-return-from lf-get--preview-create buf))
      (save-excursion
        (when (string= cmd "*Preview-Disable*")
          (insert (format "Preview Disabled for %s." entry))
          (cl-return-from lf-get--preview-create buf))
        (cl-dolist (fmt `((,entry . "%i") (,size . "%s")))
          (setq args (cl-substitute (car fmt) (cdr fmt) args :test 'string=)))
        (unless cache
          (apply #'call-process cmd nil t nil args)
          (when (string= cmd "exa")
            (ansi-color-apply-on-region
             (point-min) (progn (goto-char (point-min)) (forward-line (frame-height)) (point))))
          (cl-return-from lf-get--preview-create buf))
        (let* ((target-raw (concat lf-cache-dir size entry))
               (target-ext (concat target-raw ".jpg"))
               (target (if (and (string= cmd "convert")
                                (< (nth 7 (file-attributes entry)) (* 1024 1024 0.5)))
                           entry target-ext)))
          (if (file-exists-p target)
              (let ((img (create-image target nil nil :max-width lf-width-img)))
                (put-image img 0) (cl-return-from lf-get--preview-create buf))
            (make-directory (file-name-directory target-raw) t)
            (cl-dolist (format `((,target-raw . "%t") (,target-ext . "%T")))
              (setq args (cl-substitute (car format) (cdr format) args :test 'string=)))
            (apply #'start-process "" (generate-new-buffer "*Lf I/O*") cmd args)
            (run-with-timer 0.5 nil (lambda () (lf-update--preview (get-buffer-window buf))))
            (insert "[Cache] Generating thumbnail..."))))
      buf)))

(cl-defun lf-preview--entry (entry)
  "Create the preview buffer of `ENTRY'."
  (unless (file-readable-p entry)
    (cl-return-from lf-preview--entry (lf-get--preview-create "File Not Readable")))
  (when (file-directory-p entry)
    (cl-return-from lf-preview--entry
      (lf-get--preview-create nil nil "exa" (list "--color=always" "-al" entry))))
  (let* ((ext (or (file-name-extension entry) ""))
         (match (lf-match--preview-exts ext))
         (inhibit-modification-hooks t)
         (auto-save-default nil)
         (delay-mode-hooks t)
         (recentf-list nil)
         (inhibit-message t))
    (when match
      (cl-return-from lf-preview--entry
        (lf-get--preview-create entry (cdr match) (caar match) (cdar match))))
    (let* ((buf (find-file-noselect entry t nil))
           (binary (with-current-buffer buf
                     (save-excursion (goto-char (point-min))
                                     (search-forward (string ?\x00) nil t 1)))))
      (when (and binary (not lf-preview-binary))
        (add-to-list 'lf-preview-buffers buf)
        (cl-return-from lf-preview--entry (lf-get--preview-create "Binary preview disabled")))
      buf)))

(defun lf-update--preview (&optional preview-window)
  "Setup lf preview window."
  (when (or lf-enable-preview preview-window)
    (let* ((orig-buffer-list (buffer-list))
           (index (or (frame-parameter nil 'lf-index-path) ""))
           (preview-buffer (lf-preview--entry index))
           (preview-window (or preview-window (frame-parameter nil 'lf-preview-window))))
      (when (window-live-p preview-window)
        (set-window-buffer preview-window preview-buffer))
      (unless (memq preview-buffer orig-buffer-list)
        (push preview-buffer lf-preview-buffers))
      (with-current-buffer preview-buffer (run-hooks 'lf-preview-win-hook)))))

;;;; Header

(defun lf-update--header ()
  "Update header string.  Make sure the length of header string
is less then `lf-width-header'."
  (with-current-buffer (frame-parameter nil 'lf-header-buffer)
    (erase-buffer)
    (let ((str (funcall lf-header-string-fn))
          (max-width (floor (/ lf-width-header lf-header-scale))))
      (while (> (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
        (setq str (substring str 0 -1)))
      (insert (concat str "\n")))
    (add-text-properties (point-min) (point-max)
                         `(display '(height ,lf-header-scale) line-spacing 0.5 line-height 1.5))))

(defun lf--header-string ()
  "Compose header string."
  (let* ((index (frame-parameter nil 'lf-index-path))
         (file-path (file-name-directory index))
         (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
         (path-regex (concat (getenv "HOME") "/\\|\\/$"))
         (path-tail (replace-regexp-in-string path-regex "" file-path))
         (file-name (file-name-nondirectory index)))
    (format "   %s %s %s" (propertize (if path-prefix-home "~" ":"))
            (propertize path-tail 'face 'dired-mark)
            (propertize file-name 'face 'font-lock-constant-face))))

;;;; Footer

(defun lf-update--footer ()
  "Show file details in echo area."
  (when (and (dired-get-filename nil t) (lf-live-p))
    (let* ((fwidth (frame-width))
           (footer (format-spec lf-footer-format (lf--footer-spec)))
           (parts (split-string footer "&&&"))
           (lhs (nth 0 parts))
           (rhs (and (> (length parts) 1) (nth 1 parts)))
           (fringe-gap (if (eq fringe-mode 0) 4 2))
           (space (- fwidth fringe-gap (length lhs)))
           (message-log-max nil)
           (msg (format (format "%%s%%%ds" space) lhs rhs)))
      (message "%s" msg))))

(defun lf--footer-spec ()
  "Echo file details."
  (let* ((entry (dired-get-filename nil t))
         (fattr (file-attributes entry))
         (file-size (format "%6s" (file-size-human-readable (nth 7 fattr))))
         (user (nth 2 fattr))
         (file-date (propertize (format-time-string "%Y-%m-%d %H:%m" (nth 5 fattr))
                                'face 'font-lock-warning-face))
         (file-perm (nth 8 fattr))
         (cur-pos (- (line-number-at-pos (point)) 2))
         (final-pos (- (line-number-at-pos (point-max)) 3))
         (index (format "%3d/%-3d" cur-pos final-pos))
         (sorting (if (string= "" lf-sort-criteria) "name" lf-sort-criteria))
         (sort-by (concat "Sort by: " sorting))
         (i/o-task (or (lf-i/o--status) ""))
         (filter (format "Filter: %s"  lf-show-hidden))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sort-by) (?w . ,space) (?t . ,i/o-task))))

;;;; Overlays

(defun lf-update--line ()
  (remove-overlays (point-min) (point-max) 'lf-line t)
  (when-let* ((pos (dired-move-to-filename nil))
              (beg (line-beginning-position))
              (end (line-beginning-position 2))
              (ol (make-overlay beg end)))
    (when lf-show-icons
      (remove-overlays beg end 'lf-icons t)
      (lf-render--icon pos 'lf-line-face))
    (overlay-put ol 'lf-line t)
    (overlay-put ol 'face 'lf-line-face)))

;;;; Icons

(defun lf-update--icons ()
  (when lf-show-icons
    (remove-overlays (point-min) (point-max) 'lf-icons t)
    (save-excursion
      (let* ((curr-line (line-number-at-pos))
             (beg (forward-line (- 0 (frame-height))))
             (end (+ curr-line (frame-height))))
        (while (and (not (eobp)) (< (line-number-at-pos) end))
          (when-let ((pos (dired-move-to-filename nil)))
            (lf-render--icon pos))
          (forward-line 1))))))

(defun lf-render--icon (pos &optional face)
  (let* ((entry (dired-get-filename 'relative 'noerror))
         (offset `(:v-adjust ,lf-icons-v-offset))
         (icon-face (or (when face `(:face ,face))
                        (when lf-icons-monochrome `(:face ,(face-at-point)))))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p entry)
                   (apply 'all-the-icons-icon-for-dir entry icon-attrs)
                 (apply 'all-the-icons-icon-for-file entry icon-attrs)))
         (icon-w/-offset (concat icon "\t"))
         (icon-str (propertize icon-w/-offset 'font-lock-face face))
         (ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'lf-icons t)
    (overlay-put ov 'after-string icon-str)))

;;; Helpers

(defun lf-display--buffer (buffer alist)
  "Try displaying `BUFFER' at one side of the selected frame. This splits the
window at the designated `side' of the frame."
  (let* ((side (cdr (assq 'side alist)))
         (window-configuration-change-hook nil)
         (window-width (or (cdr (assq 'window-width alist)) 0.5))
         (size (ceiling (* (frame-width) window-width)))
         (split-width-threshold 0)
         (new-window (split-window-no-error lf-window size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun lf-get--parent (path)
  "Get parent directory of `PATH'"
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun lf-get--filesize (fileset)
  "Determine file size of provided list of files in `FILESET'."
  (unless (executable-find "du") (error "`du' executable not found."))
  (with-temp-buffer (apply 'call-process "du" nil t nil "-sch" fileset)
                    (format "%s" (progn (re-search-backward "\\(^[0-9.,]+[a-zA-Z]+\\).*total$")
                                        (match-string 1)))))

(defun lf-get--trash-dir ()
  "Get trash directory for current disk."
  (cl-dolist (dir lf-trash-dir-alist)
    (when (string-prefix-p (car dir) (dired-current-directory))
      (cl-return (concat (car dir) (cdr dir))))))

(defun lf-i/o--status ()
  (when-let* ((task (car-safe lf-i/o-queue))
              (io-buf (car task))
              (length (cddr task))
              (size (cadr task))
              (finished
               (with-current-buffer io-buf
                 (how-many "Process \\(<[0-9]+>\\)? \\(exited\\|finished\\).*"
                           (point-min) (point-max)))))
    (when (eq finished length)
      (cancel-timer (symbol-value 'lf-update--footer-timer))
      (when (lf-live-p) (revert-buffer))
      (setq lf-i/o-queue (cdr lf-i/o-queue)))
    (format "%s: %s total size: %s"
            (if (eq finished length) "Success" "Progress")
            (propertize (format "%s / %s" finished length) 'face 'font-lock-keyword-face)
            (propertize size 'face 'font-lock-builtin-face))))

(cl-defun lf-match--preview-exts (ext)
  "To determine if `EXT' can be matched by `lf-preview-cmd-alist'."
  (pcase-dolist (`(,cache ,exts ,cmd) lf-preview-cmd-alist)
    (if (listp exts)
        (when (member ext exts) (cl-return-from lf-match--preview-exts (cons cmd cache)))
      (when (string-prefix-p exts (mailcap-extension-to-mime ext))
        (cl-return-from lf-match--preview-exts (cons cmd cache))))))

(defmacro lf-define--async (func delay &optional interval)
  "Define a delayed version of FUNC-SYM with delay time DELAY.
When called, a delayed function only runs after the idle time
specified by DELAY. Multiple calls to the same function before
the idle timer fires are ignored."
  (let* ((new-func (intern (format "%s-async" func)))
         (timer (intern (format "%s-timer" func)))
         (once `(lambda () (ignore-errors (,func)) (setq ,timer nil))))
    `(progn
       (defvar ,timer nil)
       (when ,interval (add-to-list 'lf-repeat-timers ',timer))
       (defun ,new-func ()
         ,(format "Delayed version of %s" func)
         (if ,interval
             (progn
               (when (timerp ,timer) (cancel-timer ,timer))
               (setq ,timer (run-with-timer ,delay ,interval ',func)))
           (unless (timerp ,timer)
             (setq ,timer (run-with-idle-timer ,delay nil ,once))))))))

(lf-define--async lf-update--preview lf-preview-delay)

;;; Commands

;;;; Navigation

(defun lf-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (lf-get--parent current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (let* ((output (shell-command-to-string "wmic logicaldisk get name"))
                 (drives (cdr (split-string output)))
                 (drive (completing-read "Select drive: " drives)))
            (when drive (lf-find-file drive))))
      (lf-find-file parent t))))

(defun lf-go-top ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-min)) (dired-next-line 1)
  (lf-next-file -1))

(defun lf-go-bottom ()
  "Move to bottom of file list"
  (interactive)
  (goto-char (point-max)) (lf-next-file 1))

(defun lf-page-down ()
  "Scroll down file list"
  (interactive)
  (lf-next-file (window-height)))

(defun lf-page-up ()
  "Scroll up file list"
  (interactive)
  (lf-next-file (- 0 (window-height))))

(transient-define-prefix lf-go ()
  "This is just an example of `lf-go' command. You can define your
own with `transient-define-prefix'."
  ["File"
   ("l" "Follow file link" (lambda () (interactive) (lf-find-file (file-truename (dired-get-filename)))))
   ("." "`lf.el' source code." (lambda () (interactive) (find-library "lf.el")))]
  ["Directory"
   ("h" "~/" (lambda () (interactive) (lf-find-file "~/")))
   ("L" "Follow dir link" (lambda () (interactive) (lf-find-file (file-truename default-directory))))]
  ["Navigation"
   ("r" "Refresh buffer" revert-buffer)
   ("g" "Go Top" lf-go-top)])

(defun lf-next-file (arg)
  "Move lines in lf and initiate updates to preview window."
  (interactive "^p")
  (dired-next-line arg)
  (cond
   ((eobp) (unless (region-active-p) (forward-line -1)))
   ((bobp) (dired-next-line 1)))
  (when (dired-move-to-filename nil)
    (set-frame-parameter nil 'lf-index-path (dired-get-filename nil t))
    (lf-update--header)
    (lf-update--footer)
    (lf-update--preview-async)))

(defun lf-prev-file (arg)
  (interactive "^p")
  (lf-next-file (- 0 arg)))

(defun lf-show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list (completing-read "Select from history: "
                          (cl-remove-duplicates (ring-elements lf-history-ring)
                                                :test (lambda (x y) (or (null y) (equal x y)))))))
  (when history (lf-find-file history)))

(defun lf-search (&optional evil-cmd isearch-cmd)
  (interactive)
  (if (featurep 'evil)
      (progn (funcall (or evil-cmd 'evil-search-forward))
             (set-frame-parameter nil 'lf-index-path (dired-get-filename nil t))
             (lf-refresh))
    (funcall (or isearch-cmd 'isearch-forward))))

(defun lf-search-next ()
  (interactive)
  (lf-search 'evil-search-next 'isearch-repeat-forward))

(defun lf-search-previous ()
  (interactive)
  (lf-search 'evil-search-previous 'isearch-repeat-backward))

;;;; Copy / Paste

(defun lf-flag-file-yank (arg &optional interactive)
  (interactive (list current-prefix-arg t))
  (let ((dired-marker-char lf-yank-marker))
    (dired-mark arg interactive)))

(defun lf-paste (&optional mode)
  "doc"
  (interactive)
  (let* ((dired-marker-char lf-yank-marker)
         (regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf lf-parent-buffers)
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (error "No files marked for paste."))
    (lf-internal-paste yanked-files mode)))

(defun lf-move ()
  "Move marked files to current directory."
  (interactive)
  (lf-paste 'move))

(defun lf-paste-as-symlink (arg)
  "Paste (yank marked) files as symlink. With
optional (\\[universal-argument]) `RELATIVE' create relative
links."
  (interactive "P")
  (if arg (lf-paste 'relalink) (lf-paste 'symlink)))

(defun lf-internal-paste (fileset mode)
  "Helper for `lf-paste'."
  (let* ((target (dired-current-directory))
         (process-connection-type nil)
         (io-buffer (generate-new-buffer "*Lf I/O*"))
         (paste-func
          (cl-case mode
            ('copy (lambda (fr to) (start-process "" io-buffer "cp" "-f" "-r" "-v" fr to)))
            ('move (lambda (fr to) (start-process "" io-buffer "mv" "-f" "-v" fr to)))
            ('symlink (lambda (fr to) (make-symbolic-link fr to)))
            ('relalink (lambda (fr to) (dired-make-relative-symlink fr to)))))
         (new-fileset ())
         overwrite abort)
    (cl-dolist (file fileset)
      (when (and (not abort) (file-exists-p file))
        (let* ((base-name (file-name-nondirectory file))
               (paste-name (concat target base-name))
               (prompt (concat base-name " exists, overwrite?: (y)es (n)o (a)ll (q)uit"))
               choice)
          (if overwrite
              (push (cons file paste-name) new-fileset)
            (if (file-exists-p paste-name)
                (let ((name~ paste-name)
                      (idx 1))
                  (setq choice (read-char-choice prompt '(?y ?n ?a ?q)))
                  (when (eq choice ?n)
                    (while (file-exists-p name~)
                      (setq name~ (concat paste-name (number-to-string idx) "~"))
                      (setq idx (1+ idx))))
                  (cl-case choice
                    ((?y ?n) (push (cons file name~) new-fileset))
                    (?a (setq overwrite t) (push (cons file name~) new-fileset))
                    (?q (setq abort t) (setq new-fileset ()))))
              (push (cons file paste-name) new-fileset))))))
    (lf-define--async lf-update--footer 0 0.01) (lf-update--footer-async)
    ;; FIXME: make it compatable with non shell cmd
    (let ((size (lf-get--filesize (mapcar #'car new-fileset)))
          (leng (length new-fileset)))
      (add-to-list 'lf-i/o-queue (cons io-buffer (cons size leng))))
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf lf-parent-buffers)
      (with-current-buffer buf (dired-unmark-all-marks)))))

;;;; Utilities

(defun lf-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq lf-show-hidden
        (cl-case lf-show-hidden
          ('all 'dot) ('dot 'lf) ('lf 'all)))
  (revert-buffer) (lf-file--filter) (lf-refresh))

(defun lf-file--filter ()
  (save-excursion
    (let* ((all-re '("^\\.?#\\|^\\.$\\|^\\.\\.$"))
           (dot-re '("^\\."))
           (method (cl-case lf-show-hidden ('lf lf-hidden-regexp)
                            ('dot dot-re)))
           (omit-re (mapconcat 'concat (append all-re method) "\\|"))
           buffer-read-only)
      (dired-mark-unmarked-files omit-re nil nil 'no-dir)
      (goto-char (point-min))
      (let ((regexp (dired-marker-regexp)))
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))))

(defun lf-sort-criteria (criteria)
  "Call sort-dired by different `CRITERIA'."
  (interactive
   (list
    (read-char-choice
     "criteria: (n/N)ame (e/E)xt (s/S)ize (t/T)ime (c/C)time "
     '(?q ?n ?N ?e ?E ?s ?S ?t ?T ?c ?C))))
  (unless (eq criteria ?q)
    (let* ((c (char-to-string criteria))
           (revp (string-equal c (upcase c)))
           (cc (downcase c))
           (sort-flag
            (cond
             ((string-equal cc "n") '("name" . ""))
             ((string-equal cc "c") '("modified" . " -c"))
             ((string-equal cc "e") '("ext" . " -X"))
             ((string-equal cc "t") '("time" . " -t"))
             ((string-equal cc "s") '("size" . " -S"))))
           (switch (concat dired-listing-switches (cdr sort-flag) (when revp " -r"))))
      (setq lf-sort-criteria (car sort-flag))
      (dired-sort-other switch)
      (lf-refresh))))

(defun lf-toggle-preview ()
  "Toggle preview of selected file."
  (interactive)
  (setq lf-enable-preview (not lf-enable-preview))
  (lf-refresh t))

(defun lf-yank-filepath (&optional dir-only)
  "Copy the current directory's (`default-directory''s) absolute path."
  (interactive "P")
  (if dir-only
      (message (kill-new (expand-file-name default-directory)))
    (message (kill-new (dired-get-filename nil t)))))

(defun lf-rename-eol ()
  (interactive)
  (end-of-line)
  (wdired-change-to-wdired-mode))

(defun lf-less-parents ()
  "Reduce number of lf parents."
  (interactive)
  (setq lf-depth (max 0 (- lf-depth 1))) (lf-refresh t))

(defun lf-more-parents ()
  "Increase number of lf parents."
  (interactive)
  (setq lf-depth (1+ lf-depth)) (lf-refresh t))

;;;###autoload
(defun lf-live-p ()
  "Util function for detecting if in lf mode."
  (memq (selected-window) lf-parent-windows))

(defun lf-new-frame ()
  "Make a new frame and launch lf."
  (interactive)
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "lf-emacs")))))
    (with-selected-frame frame (lf))))

;;;; File open commands

(defun lf-open-file (mode)
  "Find file in lf buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in lf. `IGNORE-HISTORY' will not update history-ring on change"
  (let ((marked-files (dired-get-marked-files)))
    (lf-quit :keep-alive)
    (cl-loop for file in marked-files do
             (let ((dir-p (file-directory-p file)))
               (when (and file (not dir-p))
                 (cl-case mode
                   ('horizontal (split-window-right) (windmove-right))
                   ('vertical (split-window-below) (windmove-down))
                   ('other (other-window 1))))
               (when file (find-file file))))))

(defun lf-open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (when-let ((marked-files (dired-get-marked-files))
             (mac (lambda (f) (shell-command (format "open \"%s\"" f))))
             (linux (lambda (f) (start-process "" nil "xdg-open" f)))
             (windows (lambda (f) (start-process "" nil "open" (replace-regexp-in-string "/" "\\" f t t))))
             (cmd (cl-case system-type ('darwin mac) ('gnu/linux linux) ('windows-nt windows))))
    (mapc `,cmd marked-files)))

;;; Init

(defun lf-init--buffer ()
  (let* ((index (number-to-string (length lf-frame-alist)))
         (header-buf (get-buffer-create (concat "*Lf Header-" index "*")))
         (preview-buf (get-buffer-create (concat "*Lf Preview-" index "*"))))
    (with-current-buffer preview-buf (setq mode-line-format nil))
    (set-frame-parameter nil 'lf-preview-buffer preview-buf)
    (set-frame-parameter nil 'lf-header-buffer header-buf)))

;;;; Advices / Hooks

(defun lf-setup--dired-buffer-advice (fn &rest args)
  "Setup the dired buffer by removing the header and filter files."
  (apply fn args)
  (remove-overlays)
  (let ((ho (make-overlay (point-min) (line-end-position)))
        (so (make-overlay (point-min) (point-max))))
    (overlay-put so 'display `(height ,(1+ lf-file-line-padding)))
    (overlay-put so 'line-spacing lf-file-line-padding)
    (overlay-put ho 'after-string "\n"))
  (lf-update--icons))

(defun lf-general--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args) (revert-buffer)
  (lf-refresh (not (eq major-mode 'lf-mode))))

(defun lf-update--line-refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (remove-overlays (point-min) (point-max) 'lf-line t)
  (when-let ((pos (dired-move-to-filename nil))
             lf-show-icons)
    (remove-overlays (1- pos) pos 'lf-icons t)
    (lf-render--icon pos))
  (apply fn args)
  (lf-update--line))

(defun lf-redisplay--icons (win pos)
  "Redisplay icons incrementally, added to `window-scroll-functions'."
  (when (and lf-show-icons (eq win lf-window)) (lf-update--icons)))

(defun lf-redisplay--frame ()
  "Refresh lf frame, added to `after-focus-change-functions'."
  (frame-focus-state)
  (when (eq major-mode 'lf-mode) (lf-refresh t)))
(defun lf-evil--cursor-advice (fn &rest args)
  (unless (and (not (eq major-mode 'wdired-mode)) (lf-live-p))
    (apply fn args)))

(defun lf-deletion--refresh-advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (lf-get--trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (lf-next-file 1))
  (lf-refresh))

(defun lf-file-open--advice (fn &rest args)
  "Advice for `find-file' and `find-file-other-window'"
  (when (lf-live-p) (lf-quit :keep-alive)) (apply fn args))

(cl-dolist (fn '(lf-next-file lf-go-top lf-go-bottom lf-flag-file-yank))
  (advice-add fn :around 'lf-update--line-refresh-advice))

(defvar lf-advice-alist
  '((files         find-file                    lf-file-open--advice)
    (files         find-file-other-window       lf-file-open--advice)
    (dired         dired-readin                 lf-setup--dired-buffer-advice)
    (dired         dired-mark                   lf-update--line-refresh-advice)
    (dired         dired-flag-file-deletion     lf-update--line-refresh-advice)
    (dired         dired-internal-do-deletions  lf-deletion--refresh-advice)
    (dired         wdired-exit                  lf-general--refresh-advice)
    (dired         wdired-finish-edit           lf-general--refresh-advice)
    (dired         wdired-abort-changes         lf-general--refresh-advice)
    (dired-aux     dired-do-kill-lines          lf-general--refresh-advice)
    (dired-aux     dired-create-directory       lf-general--refresh-advice)
    (dired-aux     dired-do-create-files        lf-general--refresh-advice)
    (dired-aux     dired-do-rename              lf-general--refresh-advice)
    (dired-aux     dired-insert-subdir          lf-general--refresh-advice)
    (dired-narrow  dired-narrow--internal       lf-general--refresh-advice)
    (find-dired    find-dired-sentinel          lf-general--refresh-advice)
    (evil          evil-refresh-cursor          lf-evil--cursor-advice)
    (lsp-mode      lsp-deferred                 ignore))
  "A list of file, adviced function, and advice function.")

(put 'dired-subdir-alist 'permanent-local t)

;;;; Core setup

(defun lf-init ()
  "Save previous window config and initialize lf."
  (add-hook 'window-scroll-functions #'lf-redisplay--icons)
  (add-function :after after-focus-change-function #'lf-redisplay--frame)
  (pcase-dolist (`(,file ,sym ,fn) lf-advice-alist)
    (with-eval-after-load file (advice-add sym :around fn)))
  (when-let* ((frame (window-frame))
              (new-lf-frame (not (assoc frame lf-frame-alist))))
    (push (cons frame (current-window-configuration)) lf-frame-alist))
  (when (window-parameter nil 'window-side) (delete-window))
  (lf-init--buffer)
  (unless lf-pre-config-saved
    (unless (posframe-workable-p) (error "Lf requires GUI emacs."))
    (when (lf-i/o--status)
      (lf-define--async lf-update--footer 0 0.1) (lf-update--footer-async))
    (unless lf-override-dired-mode
      (cl-dolist (buf (buffer-list))
        (when (eq 'dired-mode (buffer-local-value 'major-mode buf))
          (kill-buffer buf))))
    (mailcap-parse-mailcaps)
    (setq mailcap-mime-extensions
          (delq (assoc ".ts" mailcap-mime-extensions) mailcap-mime-extensions))
    (add-to-list 'mailcap-mime-extensions '(".ape" . "audio/monkey"))
    (setq lf-pre-arev-mode (bound-and-true-p auto-revert-mode))
    (setq lf-pre-config-saved t)))

(defun lf-deinit ()
  "Revert previous window config and deinit lf."
  (when-let* ((config (cdr-safe (assoc (window-frame) lf-frame-alist)))
              (valid (window-configuration-p config)))
    (set-window-configuration config))
  (unless lf-pre-arev-mode (auto-revert-mode -1))
  (setq lf-pre-config-saved nil)
  (mapc #'kill-buffer lf-preview-buffers)
  (posframe-delete (frame-parameter nil 'lf-header-buffer))
  (set-frame-parameter nil 'lf-header--frame nil)
  (when-let ((singleton (< (length lf-frame-alist) 2)))
    (remove-hook 'window-scroll-functions #'lf-redisplay--icons)
    (remove-function after-focus-change-function #'lf-redisplay--frame)
    (pcase-dolist (`(,file ,sym ,fn) lf-advice-alist)
      (with-eval-after-load file (advice-remove sym fn)))
    (cl-dolist (buf (buffer-list))
      (let ((name (buffer-name buf))
            (mode (buffer-local-value 'major-mode buf)))
        (when (or (eq 'dired-mode mode) (eq 'lf-mode mode)
                  (and (not (string-equal name ""))
                       (string-match lf-cleanup-regex name)
                       (not (get-buffer-process buf))))
          (kill-buffer buf))))
    (cl-dolist (tm lf-repeat-timers) (cancel-timer (symbol-value tm))))
  (set-frame-parameter nil 'lf-preview-window nil)
  (setq lf-frame-alist (delq (assoc (window-frame) lf-frame-alist) lf-frame-alist))
  (setq lf-window nil)
  (setq lf-parent-windows ())
  (setq lf-preview-buffers ())
  (setq lf-parent-buffers ()))

(defun lf-refresh (&optional rebuild)
  "Reset lf. With optional prefix ARG (\\[universal-argument])
also rebuild lf layout."
  (interactive "P")
  (when rebuild
    (lf-build--parent-windows)
    (lf-build--preview-window)
    (lf-build--header-frame))
  (lf-update--preview)
  (lf-update--header)
  (lf-update--footer)
  (lf-update--icons)
  (lf-update--line))

;;;; Core utilities

;;;###autoload
(defun lf (&optional path)
  "Launch dired in lf-mode."
  (interactive)
  (lf-init)
  (let* ((file (or path buffer-file-name))
         (dir (if file (file-name-directory file) default-directory)))
    (lf-find-file dir)))

(defun lf-find-file (&optional file ignore-history)
  "Find file in lf buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in lf. `IGNORE-HISTORY' will not update history-ring on change"
  (interactive)
  (let ((entry (or file (dired-get-filename nil t)))
        (bname (buffer-file-name (current-buffer)))
        (curr-dir (expand-file-name default-directory)))
    (when entry
      (if (file-directory-p entry)
          (progn
            (unless ignore-history
              (when (or (ring-empty-p lf-history-ring)
                        (not (eq entry (ring-ref lf-history-ring 0))))
                (ring-insert lf-history-ring (directory-file-name entry))))
            (switch-to-buffer (or (car (dired-buffers-for-dir entry))
                                  (dired-noselect entry)))
            (setq lf-child-entry (or bname curr-dir))
            (set-frame-parameter nil 'lf-index-path (or (dired-get-filename nil t) entry))
            (lf-refresh t))
        (find-file entry)))))

(defun lf-quit (&optional keep-alive)
  "Revert lf settings and disable lf."
  (interactive)
  (lf-deinit)
  (when (and (not keep-alive)
             (string= (frame-parameter nil 'name) "lf-emacs"))
    (delete-frame)))

;;;###autoload
(define-minor-mode lf-override-dired-mode
  "Toggle lf to override dired whenever in lf-mode."
  :group 'lf :global t
  (let ((overrider '(lambda () (unless (lf-live-p) (lf)))))
    (if lf-override-dired-mode
        (add-hook 'dired-mode-hook overrider)
      (remove-hook 'dired-mode-hook overrider))))

(define-derived-mode lf-mode dired-mode "Lf"
  "Major mode emulating the lf file manager in `dired'."
  :group 'lf
  :interactive nil)

(provide 'lf)

;;; lf.el ends here
