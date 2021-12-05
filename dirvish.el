;;; dirvish.el --- a modern file manager based on dired mode. -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Package-Version: 0.7.0
;; Keywords: ranger, file, dired
;; Homepage: https://github.com/alexluigit/dirvish.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26") (posframe "1.1.1"))

;;; Commentary:

;; `dirvish.el' is a minimalistic file manager based on `dired-mode'.  It is inspired by ranger (see
;; https://github.com/ranger/ranger), which is a terminal file manager that shows a stack of the
;; parent directories, and updates its parent buffers while navigating the file system with an
;; optional preview window at side showing the content of the selected file.

;; Unlike `ranger.el', which tries to become an all-around emulation of ranger, dirvish.el is more
;; bare-bone, meaning it does NOT try to port all "goodness" from ranger, instead, it tries to:
;;
;;   - provides a better dired UI
;;   - make some dired commands more intuitive
;;   - keep all your dired key bindings
;;
;; The name `dirvish' is a tribute to `vim-dirvish'.

;;; Code:

(autoload 'format-spec "format-spec")
(declare-function image-get-display-property "image-mode")
(require 'ring)
(require 'posframe)
(require 'dired-x)
(require 'ansi-color)
(require 'mailcap)
(eval-when-compile (require 'subr-x))

(defgroup dirvish nil
  "A better dired."
  :group 'dired)

;;; Variables

;;;; User facing options

(defcustom dirvish-show-hidden 'all
  "Determine hidden method in dirvish."
  :group 'dirvish
  :type '(radio (const :tag "Show All Files" :value 'all)
                (const :tag "Hide Common Files" :value 'dirvish)
                (const :tag "Hide All Dotfiles" :value 'dot)))

(defcustom dirvish-hidden-regexp
  '("^\\.\\(git\\|hg\\|svn\\)$"
    "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
    "~$" "^#.*#$")
  "Regexp of custom filetypes to omit in dirvish."
  :group 'dirvish :type 'list)

(defcustom dirvish-preview-cmd-alist
  '(("text/"                   (find-file-noselect . (t nil)))
    ("image/"                  ("convert" "-resize" "%s" "%i" "%T"))
    ("audio/"                  ("mediainfo" "%i"))
    ("video/"                  ("ffmpegthumbnailer" "-i" "%i" "-o" "%T" "-s 0"))
    (("iso" "bin" "exe" "gpg") ("*Preview Disable*"))
    (("zip")                   ("zipinfo" "%i"))
    (("zst" "tar")             ("tar" "-tvf" "%i"))
    (("epub")                  ("epub-thumbnailer" "%i" "%T" "1024"))
    (("pdf")                   ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" "%i" "%t")))
  "doc"
  :group 'dirvish :type '(alist :value-type ((choice list string) list)))

(defcustom dirvish-history-length 30
  "Length of history dirvish will track."
  :group 'dirvish :type 'integer)

(defcustom dirvish-cache-dir
  (concat (or (getenv "XDG_CACHE_HOME") (concat (getenv "HOME") "/.cache")) "/dirvish/")
  "Preview / thumbnail cache directory for dirvish."
  :group 'dirvish :type 'string)

(defcustom dirvish-depth 1
  "Number of directories up to traverse."
  :group 'dirvish :type 'integer)

(defcustom dirvish-enable-preview t
  "When not-nil preview the selected file."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-width-parents 0.12
  "Fraction of frame width taken by parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-max-parent-width 0.25
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'dirvish :type 'float)

(defcustom dirvish-line-padding 0.1
  "doc"
  :group 'dirvish :type 'float)

(defcustom dirvish-header-string-fn 'dirvish--header-string
  "Function used to output a string that will show up as header."
  :group 'dirvish :type 'function)

(defcustom dirvish-header-position
  (lambda (_)
    (let ((tab-h (tab-bar-height nil t))
          (fringe (or (frame-parameter nil 'internal-border-width) 0)))
      (cons 0 (+ tab-h fringe))))
  "doc"
  :group 'dirvish :type 'function)

(defcustom dirvish-footer-format "Sort: %S  Filter: %f  %d  %p%w%t %i"
  "Format for footer display. "
  :group 'dirvish :type 'string)

(defcustom dirvish-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR) where DISK is path to a disk and
TRASH-DIR is corresponding trash directory."
  :group 'dirvish :type 'alist)

(defcustom dirvish-show-icons t
  "When not-nil show file / dir icon."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-icons-monochrome t
  "doc"
  :group 'dirvish :type 'boolean)

(defcustom dirvish-icons-v-offset 0.01
  "doc"
  :group 'dirvish :type 'float)

;;;; Faces

(defgroup dirvish-faces nil
  "Faces used by Dirvish."
  :group 'dirvish :group 'faces)

(defface dirvish-line-face
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Face used for `dirvish-update--line'."
  :group 'dirvish-faces)

;;;; Compiler

(defvar recentf-list)
(defvar dirvish-update--preview-timer)
(defvar dirvish-override-dired-mode)
(defvar posframe-mouse-banish)
(defvar image-mode-map)
(setq posframe-mouse-banish nil)

;;;; Internal variables

(defvar dirvish-width-img nil
  "Calculated preview window width. Used for image preview.")

(defvar dirvish-width-header nil
  "Calculated header frame width.")

(defvar dirvish-header-scale 1.25
  "Height of header line.")

(defvar dirvish-history-ring (make-ring dirvish-history-length)
  "History for `dirvish-find-file'.")

(defvar dirvish-initialized nil
  "Indicate if previous window config saved.")

(defvar dirvish-orig-recentf-list nil
  "doc")

(defvar dirvish-preview-delay 0.02
  "Time in seconds to delay running preview file functions.")

(defvar dirvish-footer-repeat 0.02
  "Time in seconds to repeat footer update.")

(defvar dirvish-sort-criteria ""
  "Default `ls' sorting switches.")

(defvar dirvish-window nil
  "Main dirvish window. Adjacent to preview window.")

(defvar dirvish-parent-windows ()
  "List of parent windows.")

(defvar dirvish-parent-buffers ()
  "List with buffers of parent buffers.")

(defvar dirvish-preview-buffers ()
  "List with buffers of previewed files.")

(defvar dirvish-frame-alist ()
  "List of frames using dirvish.")

(defvar dirvish-mode-hook nil
  "Hooks for setting dirvish parent windows.")

(defvar dirvish-preview-setup-hook nil
  "Hooks for setting dirvish preview windows.")

(defvar dirvish-repeat-timers '()
  "Timers with repeat flag need to be clean when exit.")

(defvar dirvish-i/o-queue ()
  "doc")

;;;; Buffer / Frame local variables

(defvar-local dirvish-child-entry nil
  "doc")

(defvar dirvish-index-path nil
  "Latest path in dirvish-window.")

(defvar dirvish-preview-window nil
  "Window contains file / directory preview.")

(defvar dirvish-preview-buffer nil
  "doc")

(defvar dirvish-header-buffer nil
  "Buffer for showing header line.")

(defvar dirvish-header--frame nil
  "Frame for showing header line.")

;;;; Keymap

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap dired-do-copy]                'dirvish-yank)
    (define-key map [remap dired-jump]                   'dirvish-jump)
    (define-key map [remap dired-do-redisplay]           'dirvish-change-level)
    (define-key map [remap dired-omit-mode]              'dirvish-toggle-dotfiles)
    (define-key map [remap dired-hide-details-mode]      'dirvish-toggle-preview)
    (define-key map [remap dired-find-file]              'dirvish-find-file)
    (define-key map [remap dired-find-alternate-file]    'dirvish-find-file)
    (define-key map [remap dired-up-directory]           'dirvish-up-directory)
    (define-key map [remap dired-next-line]              'dirvish-next-file)
    (define-key map [remap dired-previous-line]          'dirvish-prev-file)
    (define-key map [remap end-of-buffer]                'dirvish-go-bottom)
    (define-key map [remap beginning-of-buffer]          'dirvish-go-top)
    (define-key map [remap dired-sort-toggle-or-edit]    'dirvish-sort-by-criteria)
    (define-key map [remap revert-buffer]                'dirvish-refresh)
    (define-key map [remap dired-view-file]              'dirvish-toggle-preview)
    (define-key map [remap mode-line-other-buffer]       'dirvish-other-buffer)
    (define-key map [remap quit-window]                  'dirvish-quit)
    (define-key map [remap delete-window]
                (lambda ()
                  (interactive)
                  (message "%s" (substitute-command-keys "Press \\[quit-window] to quit dirvish"))))
    map)
  "Dirvish mode map.")

(with-eval-after-load 'image-mode
  (define-key image-mode-map [remap image-next-file] (lambda () (interactive) (dirvish) (dirvish-next-file 1)))
  (define-key image-mode-map [remap image-previous-file] (lambda () (interactive) (dirvish) (dirvish-next-file -1))))

;;; Layout

;;;; Header frame

(defun dirvish-width-header ()
  "Calculate header frame width. Default to frame width when disable preview."
  (* (frame-width) (if dirvish-enable-preview (- 1 dirvish-width-preview) 1)))

(cl-defun dirvish-build--header-frame ()
  (when-let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (cl-return-from dirvish-build--header-frame))
  (let* ((buf (frame-parameter nil 'dirvish-header-buffer))
         (min-w (1+ (ceiling (dirvish-width-header))))
         (f-props `(:background-color
                    ,(face-attribute 'region :background)
                    :poshandler ,dirvish-header-position
                    :min-width ,min-w
                    :min-height 2))
         (h-frame (frame-parameter nil 'dirvish-header--frame))
         (size `(:posframe ,h-frame :height 2 :max-height 2 :min-height 2
                           :width: ,min-w :min-width ,min-w :max-width ,min-w)))
    (setq dirvish-width-header min-w)
    (if h-frame
        (posframe--set-frame-size size)
      (let ((fr (apply #'posframe-show buf f-props)))
        (set-frame-parameter nil 'dirvish-header--frame fr)))))

;;;; Parent windows

(defun dirvish-build--parent-windows ()
  (cl-flet ((setup (child win buf one-w)
              (when child (dired-goto-file child))
              (add-to-list 'dirvish-parent-windows win)
              (add-to-list 'dirvish-parent-buffers buf)
              (dirvish-mode)
              (dirvish-setup--header (if one-w 'one-window 'posframe))))
    (let* ((current (expand-file-name default-directory))
           (parent (dirvish-get--parent current))
           (parent-dirs ())
           (one-window (frame-parameter nil 'dirvish-one-window))
           (depth dirvish-depth)
           (i 0))
      (setq dirvish-window (frame-selected-window))
      (if one-window (setq depth 0) (delete-other-windows))
      (setup dirvish-child-entry dirvish-window (current-buffer) one-window)
      (while (and (< i depth) (not (string= current parent)))
        (setq i (+ i 1))
        (push (cons current parent) parent-dirs)
        (setq current (dirvish-get--parent current))
        (setq parent (dirvish-get--parent parent)))
      (when (> depth 0)
        (let ((width (min (/ dirvish-max-parent-width depth) dirvish-width-parents)))
          (cl-dolist (parent-dir parent-dirs)
            (let* ((current (car parent-dir))
                   (parent (cdr parent-dir))
                   (win-alist `((side . left)
                                (inhibit-same-window . t)
                                (window-width . ,width)))
                   (buffer (dired-noselect parent))
                   (window (display-buffer buffer `(dirvish-display--buffer . ,win-alist))))
              (with-selected-window window
                (setup current window buffer one-window)
                (dired-hide-details-mode t)
                (dirvish-update--padding)
                (dirvish-update--icons)
                (dirvish-update--line))))))
      (when dirvish-enable-preview (dired-hide-details-mode t)))))

;;;; Preview window

(cl-defun dirvish-build--preview-window ()
  (when-let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (cl-return-from dirvish-build--preview-window))
  (when dirvish-enable-preview
    (let* ((inhibit-modification-hooks t)
           (win-alist `((side . right) (window-width . ,dirvish-width-preview)))
           (buf (frame-parameter nil 'dirvish-preview-buffer))
           (fringe 30)
           (new-window (display-buffer buf `(dirvish-display--buffer . ,win-alist))))
      (set-window-fringes new-window fringe fringe nil t)
      (setq dirvish-width-img (window-width new-window t))
      (set-frame-parameter nil 'dirvish-preview-window new-window))))

;;; Update

;;;; Preview

(defun dirvish--preview-process-sentinel (proc _exit)
  "doc"
  (let ((buf (frame-parameter nil 'dirvish-preview-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer (frame-parameter nil 'dirvish-preview-buffer)
        (erase-buffer) (remove-overlays)
        (let ((result-str (with-current-buffer (process-buffer proc) (buffer-string))))
          (insert result-str)
          (ansi-color-apply-on-region
           (point-min) (progn (goto-char (point-min)) (forward-line (frame-height)) (point))))))))

(cl-defun dirvish-get--preview-create (entry &optional cmd args)
  "Get corresponding preview buffer."
  (let ((buf (frame-parameter nil 'dirvish-preview-buffer))
        (process-connection-type nil)
        (size (number-to-string (or (and (boundp 'dirvish-minibuf-preview--width)
                                         dirvish-minibuf-preview--width)
                                    dirvish-width-img)))
        cache)
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (unless cmd (insert entry) (cl-return-from dirvish-get--preview-create buf))
      (when (string= cmd "*Preview Disable*")
        (insert (format "Preview Disabled for %s." entry))
        (cl-return-from dirvish-get--preview-create buf))
      (unless (executable-find cmd)
        (insert (format "Install `%s' to preview %s" cmd entry))
        (cl-return-from dirvish-get--preview-create buf))
      (when (or (member "%T" args) (member "%t" args)) (setq cache t))
      (cl-dolist (fmt `((,entry . "%i") (,size . "%s")))
        (setq args (cl-substitute (car fmt) (cdr fmt) args :test 'string=)))
      (unless cache
        (let* ((process-connection-type nil)
               (default-directory "~") ; Avoid "Setting current directory" error after deleting dir
               (res-buf (get-buffer-create " *Dirvish preview result*"))
               (proc (apply 'start-process "dirvish-preview-process" res-buf cmd args)))
          (with-current-buffer res-buf (erase-buffer) (remove-overlays))
          (set-process-sentinel proc 'dirvish--preview-process-sentinel))
        (cl-return-from dirvish-get--preview-create buf))
      (save-excursion
        ;; FIXME: a better way to deal with gif?
        (when (string= (mailcap-file-name-to-mime-type entry) "image/gif")
          (let ((gif-buf (find-file-noselect entry t nil))
                (callback (lambda (buf)
                            (when (buffer-live-p buf)
                              (with-current-buffer buf
                                (image-animate (image-get-display-property)))))))
            (run-with-idle-timer 1 nil callback gif-buf)
            (cl-return-from dirvish-get--preview-create gif-buf)))
        (let* ((target-raw (concat dirvish-cache-dir size entry))
               (target-ext (concat target-raw ".jpg"))
               (target (if (and (string= cmd "convert")
                                (< (nth 7 (file-attributes entry)) (* 1024 1024 0.5)))
                           entry target-ext)))
          (if (file-exists-p target)
              (let ((img (create-image target nil nil :max-width dirvish-width-img)))
                (put-image img 0) (cl-return-from dirvish-get--preview-create buf))
            (make-directory (file-name-directory target-raw) t)
            (cl-dolist (format `((,target-raw . "%t") (,target-ext . "%T")))
              (setq args (cl-substitute (car format) (cdr format) args :test 'string=)))
            (let ((proc (apply 'start-process "dirvish-preview-process" buf cmd args)))
              (set-process-sentinel proc (lambda (_p _e) (dirvish-update--preview))))
            (insert "[Cache] Generating thumbnail..."))))
      buf)))

(cl-defun dirvish-match--preview-mime (file)
  "To determine if `FILE' can be matched by `dirvish-preview-cmd-alist'."
  (pcase-dolist (`(,re-or-exts ,cmd) dirvish-preview-cmd-alist)
    (if (listp re-or-exts)
        (let ((ext (file-name-extension file)))
          (when (member ext re-or-exts)
            (cl-return-from dirvish-match--preview-mime cmd)))
      (when (string-match re-or-exts (or (mailcap-file-name-to-mime-type file) ""))
        (cl-return-from dirvish-match--preview-mime cmd)))))

(cl-defun dirvish-preview--entry (entry)
  "Create the preview buffer of `ENTRY'."
  (unless (file-readable-p entry)
    (cl-return-from dirvish-preview--entry
      (dirvish-get--preview-create "File Not Readable")))
  (when (file-directory-p entry)
    (cl-return-from dirvish-preview--entry
      (dirvish-get--preview-create "directory" "exa" (list "--color=always" "-al" entry))))
  (let ((match (dirvish-match--preview-mime entry))
        (enable-local-variables nil)
        (inhibit-modification-hooks t)
        (auto-save-default nil)
        (delay-mode-hooks t)
        (inhibit-message t))
    (if match
        (let ((cmd (car match)) (args (cdr match)))
          (when (functionp cmd)
            (cl-return-from dirvish-preview--entry (apply cmd entry args)))
          (dirvish-get--preview-create entry cmd args))
      (let ((threshold (or large-file-warning-threshold 10000000))
            (filesize (file-attribute-size (file-attributes entry))))
        (if (> filesize threshold)
            (dirvish-get--preview-create (concat entry " too big for literal preview"))
          (find-file-noselect entry t nil))))))

(defun dirvish-update--preview (&optional preview-window)
  "Setup dirvish preview window."
  (when (or (and dirvish-enable-preview
                 (not (frame-parameter nil 'dirvish-one-window)))
            preview-window)
    (let* ((orig-buffer-list (buffer-list))
           (index (or (frame-parameter nil 'dirvish-index-path) ""))
           (preview-buffer (dirvish-preview--entry index))
           (preview-window (or preview-window (frame-parameter nil 'dirvish-preview-window))))
      (when (window-live-p preview-window)
        (set-window-buffer preview-window preview-buffer))
      (unless (memq preview-buffer orig-buffer-list)
        (push preview-buffer dirvish-preview-buffers))
      (with-current-buffer preview-buffer (run-hooks 'dirvish-preview-setup-hook)))))

;;;; Header / Footer / Filter

(defun dirvish-update--header ()
  "Update header string.  Make sure the length of header string
is less then `dirvish-width-header'."
  (if-let ((one-window (frame-parameter nil 'dirvish-one-window)))
      (dirvish-setup--header 'one-window)
    (with-current-buffer (frame-parameter nil 'dirvish-header-buffer)
      (erase-buffer)
      (let ((str (funcall dirvish-header-string-fn))
            (max-width (1- (floor (/ dirvish-width-header dirvish-header-scale)))))
        (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
          (setq str (substring str 0 -1)))
        (insert (concat str "\n")))
      (add-text-properties (point-min) (point-max)
                           `(display '(height ,dirvish-header-scale) line-spacing 0.5 line-height 1.5)))))

(defun dirvish--header-string ()
  "Compose header string."
  (let* ((index (frame-parameter nil 'dirvish-index-path))
         (file-path (file-name-directory index))
         (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
         (path-regex (concat (getenv "HOME") "/\\|\\/$"))
         (path-tail (replace-regexp-in-string path-regex "" file-path))
         (file-name (file-name-nondirectory index)))
    (format "   %s %s %s" (propertize (if path-prefix-home "~" ":"))
            (propertize path-tail 'face 'dired-mark)
            (propertize file-name 'face 'font-lock-constant-face))))

(cl-defun dirvish-update--footer ()
  "Show file details in echo area."
  (when-let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (cl-return-from dirvish-update--footer))
  (when (and (dired-get-filename nil t) (dirvish-live-p))
    (let* ((fwidth (frame-width))
           (footer (format-spec dirvish-footer-format (dirvish--footer-spec)))
           (parts (split-string footer "&&&"))
           (lhs (nth 0 parts))
           (rhs (and (> (length parts) 1) (nth 1 parts)))
           (fringe-gap (if (eq fringe-mode 0) 4 2))
           (space (- fwidth fringe-gap (length lhs)))
           (message-log-max nil)
           (msg (format (format "%%s%%%ds" space) lhs rhs)))
      (message "%s" msg))))

(defun dirvish--footer-spec ()
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
         (sorting (if (string= "" dirvish-sort-criteria) "name" dirvish-sort-criteria))
         (i/o-task (or (dirvish-get--i/o-status) ""))
         (filter (format "%s" dirvish-show-hidden))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sorting) (?w . ,space) (?t . ,i/o-task))))

(defun dirvish-update--filter ()
  (save-excursion
    (let* ((all-re '("^\\.?#\\|^\\.$\\|^\\.\\.$"))
           (dot-re '("^\\."))
           (method (cl-case dirvish-show-hidden ('dirvish dirvish-hidden-regexp)
                            ('dot dot-re)))
           (omit-re (mapconcat 'concat (append all-re method) "\\|"))
           buffer-read-only)
      (dired-mark-unmarked-files omit-re nil nil 'no-dir)
      (goto-char (point-min))
      (let ((regexp (dired-marker-regexp)))
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))))

;;;; Overlays / Viewport

(defun dirvish-update--padding ()
  (save-excursion
    (remove-overlays)
    (let ((o (make-overlay (point-min) (point-max))))
      (setq line-spacing dirvish-line-padding)
      (overlay-put o 'display `(height ,(1+ dirvish-line-padding))))))

(defun dirvish-update--line ()
  (remove-overlays (point-min) (point-max) 'dirvish-line t)
  (when-let* ((pos (dired-move-to-filename nil))
              (beg (line-beginning-position))
              (end (line-beginning-position 2))
              (ol (make-overlay beg end)))
    (when dirvish-show-icons
      (remove-overlays beg end 'dirvish-icons t)
      (dirvish-render--icon pos 'dirvish-line-face))
    (overlay-put ol 'dirvish-line t)
    (overlay-put ol 'face 'dirvish-line-face)))

(defun dirvish-update--icons ()
  (when dirvish-show-icons
    (remove-overlays (point-min) (point-max) 'dirvish-icons t)
    (dirvish-update--attribute 'dirvish-render--icon)))

(defun dirvish-render--icon (pos &optional face)
  (let* ((entry (dired-get-filename 'relative 'noerror))
         (offset `(:v-adjust ,dirvish-icons-v-offset))
         (icon-face (or (when face `(:face ,face))
                        (when dirvish-icons-monochrome `(:face ,(face-at-point)))))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p entry)
                   (apply 'all-the-icons-icon-for-dir entry icon-attrs)
                 (apply 'all-the-icons-icon-for-file entry icon-attrs)))
         (icon-w/-offset (concat icon "\t"))
         (icon-str (propertize icon-w/-offset 'font-lock-face face))
         (ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dirvish-icons t)
    (overlay-put ov 'after-string icon-str)))

(defun dirvish-update--attribute (render-func &optional range)
  "doc"
  (save-excursion
    (let ((beg (or (car range) (- 0 (frame-height))))
          (end (or (cdr range) (+ (line-number-at-pos) (frame-height)))))
      (forward-line beg)
      (while (and (not (eobp)) (< (line-number-at-pos) end))
        (when-let ((pos (dired-move-to-filename nil)))
          (funcall render-func pos))
        (forward-line 1)))))

;;; Helpers

(defun dirvish-display--buffer (buffer alist)
  "Try displaying `BUFFER' at one side of the selected frame. This splits the
window at the designated `side' of the frame."
  (let* ((side (cdr (assq 'side alist)))
         (window-configuration-change-hook nil)
         (window-width (or (cdr (assq 'window-width alist)) 0.5))
         (size (ceiling (* (frame-width) window-width)))
         (split-width-threshold 0)
         (new-window (split-window-no-error dirvish-window size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun dirvish-get--parent (path)
  "Get parent directory of `PATH'"
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun dirvish-get--filesize (fileset)
  "Determine file size of provided list of files in `FILESET'."
  (unless (executable-find "du") (user-error "`du' executable not found."))
  (with-temp-buffer
    (apply 'call-process "du" nil t nil "-sch" fileset)
    (format "%s" (progn (re-search-backward "\\(^[0-9.,]+[a-zA-Z]*\\).*total$")
                        (match-string 1)))))

(defun dirvish-get--trash-dir ()
  "Get trash directory for current disk."
  (cl-dolist (dir dirvish-trash-dir-alist)
    (when (string-prefix-p (car dir) (dired-current-directory))
      (cl-return (concat (car dir) (cdr dir))))))

(defun dirvish-get--i/o-status ()
  (when-let* ((task (car-safe dirvish-i/o-queue)))
    (let ((finished (car task))
          (size (nth 2 task))
          (index (car (nth 3 task)))
          (length (cdr (nth 3 task))))
      (when finished
        (setq dirvish-i/o-queue (cdr dirvish-i/o-queue))
        (unless dirvish-i/o-queue
          (cancel-timer (symbol-value 'dirvish-update--footer-timer))))
      (format "%s: %s total size: %s"
              (if finished "Success" "Progress")
              (propertize (format "%s / %s" index length) 'face 'font-lock-keyword-face)
              (propertize size 'face 'font-lock-builtin-face)))))

(defun dirvish-set--i/o-status ()
  (when-let* ((task (car-safe dirvish-i/o-queue)))
    (let ((io-buf (nth 1 task))
          (progress (car (nth 3 task)))
          (length (cdr (nth 3 task)))
          (mode (nth 4 task))
          (proc-exit "Process \\(<[0-9]+>\\)? \\(exited\\|finished\\).*"))
      (if (or (eq mode 'copy) (eq mode 'move))
          (setq progress (with-current-buffer io-buf
                           (how-many proc-exit (point-min) (point-max))))
        (setq progress length))
      (when (eq progress length)
        (when (dirvish-live-p) (dirvish-refresh))
        (setf (nth 0 (car-safe dirvish-i/o-queue)) t)
        (when (eq (length dirvish-i/o-queue) 1)
          (cancel-timer (symbol-value 'dirvish-set--i/o-status-timer))))
      (setcar (nth 3 (car-safe dirvish-i/o-queue)) progress))))

(defun dirvish-override-dired (&rest _)
  "Helper func for `dirvish-override-dired-mode'."
  (dirvish nil (or (not window-system)
                  (not (= (length (window-list)) 1)))))

(defmacro dirvish-repeat (func delay interval &rest args)
  "doc"
  (let ((timer (intern (format "%s-timer" func))))
    `(progn
       (defvar ,timer nil)
       (add-to-list 'dirvish-repeat-timers ',timer)
       (setq ,timer (run-with-timer ,delay ,interval ',func ,@args)))))

(defmacro dirvish-debounce (func delay &rest args)
  "Execute a delayed version of FUNC with delay time DELAY.
When called, the FUNC only runs after the idle time
specified by DELAY. Multiple calls to the same function before
the idle timer fires are ignored."
  (let* ((timer (intern (format "%s-timer" func)))
         (do-once `(lambda (&rest args)
                     (unwind-protect (apply #',func args) (setq ,timer nil)))))
    `(progn
       (unless (boundp ',timer) (defvar ,timer nil))
       (unless (timerp ,timer)
         (setq ,timer (run-with-idle-timer ,delay nil ,do-once ,@args))))))

;;; Commands

;;;; Navigation

(defun dirvish-other-buffer ()
  "Replacement for `mode-line-other-buffer' in dirvish-mode."
  (interactive)
  (let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (if one-window
        (switch-to-buffer (other-buffer) nil t)
      (dirvish-find-file (ring-ref dirvish-history-ring 1)))))

(defun dirvish-jump (file)
  "Replacement for `dired-jump'"
  (interactive (list (read-file-name "Jump to: "))) (dirvish-find-file-dwim file))

(defun dirvish-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish-get--parent current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (let* ((output (shell-command-to-string "wmic logicaldisk get name"))
                 (drives (cdr (split-string output)))
                 (drive (completing-read "Select drive: " drives)))
            (when drive (dirvish-find-file drive))))
      (dirvish-find-file parent t))))

(defun dirvish-go-top ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-min)) (dired-next-line 1)
  (dirvish-next-file -1))

(defun dirvish-go-bottom ()
  "Move to bottom of file list"
  (interactive)
  (goto-char (point-max)) (dirvish-next-file 1))

(defun dirvish-next-file (arg)
  "Move lines in dirvish and initiate updates to preview window."
  (interactive "^p")
  (dired-next-line arg)
  (cond
   ((eobp) (unless (region-active-p) (forward-line -1)))
   ((bobp) (dired-next-line 1)))
  (when (dired-move-to-filename nil)
    (set-frame-parameter nil 'dirvish-index-path (dired-get-filename nil t))
    (dirvish-update--header)
    (dirvish-update--footer)
    (dirvish-debounce dirvish-update--preview dirvish-preview-delay)))

(defun dirvish-prev-file (arg)
  (interactive "^p")
  (dirvish-next-file (- 0 arg)))

(defun dirvish-show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list (completing-read "Select from history: "
                          (cl-remove-duplicates (ring-elements dirvish-history-ring)
                                                :test (lambda (x y) (or (null y) (equal x y)))))))
  (when history (dirvish-find-file history)))

;;;; Copy / Paste

(defun dirvish-paste (&optional mode)
  "doc"
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter 'buffer-live-p dirvish-parent-buffers))
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (error "No files marked for paste."))
    (dirvish-internal-paste yanked-files mode)))

(defun dirvish-internal-paste (fileset mode)
  "Helper for `dirvish-paste'."
  (let* ((target (dired-current-directory))
         (process-connection-type nil)
         (io-buffer (generate-new-buffer " *Dirvish I/O*"))
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
    (let ((size (dirvish-get--filesize (mapcar #'car new-fileset)))
          (leng (length new-fileset)))
      (add-to-list 'dirvish-i/o-queue `(nil ,io-buffer ,size ,(cons 0 leng) ,mode)))
    (dirvish-repeat dirvish-update--footer 0 0.1)
    (dirvish-repeat dirvish-set--i/o-status 0 0.1)
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf dirvish-parent-buffers)
      (with-current-buffer buf (dired-unmark-all-marks)))))

;;;; Utilities

(defun dirvish-yank (&optional arg)
  (interactive "P")
  (if arg (dirvish-paste 'move) (dirvish-paste)))

(defun dirvish-change-level (&optional arg)
  (interactive "p")
  (setq dirvish-depth (or arg 1)) (dirvish-refresh t))

(defun dirvish-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq dirvish-show-hidden
        (cl-case dirvish-show-hidden
          ('all 'dot) ('dot 'dirvish) ('dirvish 'all)))
  (dirvish-refresh nil t))

(defun dirvish-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq dirvish-enable-preview (not dirvish-enable-preview))
  (dirvish-refresh t)
  (when dirvish-enable-preview
    (dired-hide-details-mode t)))

(defun dirvish-sort-by-criteria (criteria)
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
      (setq dirvish-sort-criteria (car sort-flag))
      (dired-sort-other switch)
      (dirvish-refresh))))

;;;###autoload
(defun dirvish-live-p (&optional win)
  "Util function for detecting if in dirvish mode."
  (memq (or win (selected-window)) dirvish-parent-windows))

(defun dirvish-new-frame (&optional path)
  "Make a new frame and launch dirvish."
  (interactive (list (read-file-name "Open in new frame: ")))
  (when (with-selected-window (selected-window) (eq major-mode 'dirvish-mode))
    (dirvish-quit))
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "dirvish-emacs") (alpha . (100 50))))))
    (with-selected-frame frame (dirvish path))))

;;; Init

(defun dirvish-init--buffer ()
  (let* ((index (number-to-string (length dirvish-frame-alist)))
         (header-buf (get-buffer-create (concat " *Dirvish Header-" index "*")))
         (preview-buf (get-buffer-create (concat " *Dirvish Preview-" index "*"))))
    (with-current-buffer preview-buf (setq mode-line-format nil))
    (with-current-buffer header-buf (setq-local face-font-rescale-alist nil))
    (set-frame-parameter nil 'dirvish-preview-buffer preview-buf)
    (set-frame-parameter nil 'dirvish-header-buffer header-buf)))

(defun dirvish-setup--header (type)
  (setq tab-line-format nil)
  (cl-case type
    ('posframe
     (setq header-line-format (propertize " " 'display `(height ,(* 2 (1+ dirvish-line-padding)))))
     (set-face-attribute 'header-line nil :box nil))
    ('one-window
     (setq header-line-format (propertize (dirvish--header-string) 'display `(height ,dirvish-header-scale)))
     (set-face-attribute 'header-line nil :box '(:line-width 4 :color "#353644")))))

;;;; Advices / Hooks

(defun dirvish-redisplay--frame ()
  "Refresh dirvish frame, added to `after-focus-change-functions'."
  (if (eq major-mode 'dirvish-mode)
      (dirvish-refresh t)
    (when (memq (previous-frame) (mapcar 'car dirvish-frame-alist))
      (with-selected-frame (previous-frame)
        (dirvish-build--header-frame)
        (dirvish-update--header)))))

(defun dirvish-setup-dired-buffer--advice (fn &rest args)
  "Remove the header line in dired buffer."
  (apply fn args)
  (save-excursion
    (let ((inhibit-read-only t))
      (delete-region (point-min) (progn (forward-line 1) (point))))))

(defun dirvish-refresh--advice (fn &rest args)
  "Apply FN with ARGS, rebuild dirvish frame when necessary."
  (apply fn args)
  (let ((rebuild (not (eq major-mode 'dirvish-mode))))
    (dirvish-refresh rebuild nil 'no-revert)))

(defun dirvish-revert--advice (fn &rest args)
  "Apply FN with ARGS then revert buffer."
  (apply fn args) (dirvish-refresh))

(defun dirvish-refresh-cursor--advice (fn &rest args)
  (unless (and (not (eq major-mode 'wdired-mode)) (dirvish-live-p))
    (apply fn args)))

(defun dirvish-update-line--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (remove-overlays (point-min) (point-max) 'dirvish-line t)
  (when-let ((pos (dired-move-to-filename nil))
             dirvish-show-icons)
    (remove-overlays (1- pos) pos 'dirvish-icons t)
    (dirvish-render--icon pos))
  (apply fn args)
  (dirvish-update--line))

(defun dirvish-deletion--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (dirvish-get--trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (dirvish-next-file 1))
  (dirvish-refresh))

(defun dirvish-file-open--advice (fn &rest args)
  "Advice for commands that open a file."
  (when (dirvish-live-p) (dirvish-quit :keep-alive))
  (let ((default-directory "")) (apply fn args)))

;; FIXME: it should support window when current instance is launched by `(dirvish nil t)'
(defun dirvish-other-window--advice (fn &rest args)
  "Open current file/dir in new frame."
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (dirvish-new-frame file)
      (apply fn args))))

(defun dirvish-get-subdir--advice (fn &rest args)
  "Don't return subdir when cursor is in first line.

See `dirvish-setup-dired-buffer--advice'."
  (unless (eq (line-number-at-pos) 1)
    (apply fn args)))

(defun dirvish-update--viewports (win _)
  "Refresh attributes in viewport, added to `window-scroll-functions'."
  (when (and (eq win dirvish-window)
             (eq (selected-frame) (window-frame dirvish-window)))
    (with-selected-window win
      (dirvish-update--icons)
      (dirvish-update--line))))

(cl-dolist (fn '(dirvish-next-file dirvish-go-top dirvish-go-bottom dirvish-flag-file-yank))
  (advice-add fn :around 'dirvish-update-line--advice))

(with-eval-after-load 'pdf-tools
  (delete '(("pdf") ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" "%i" "%t"))
          dirvish-preview-cmd-alist))

(defvar dirvish-advice-alist
  '((files         find-file                    dirvish-file-open--advice)
    (files         find-file-other-window       dirvish-file-open--advice)
    (dired         dired-find-file-other-window dirvish-other-window--advice)
    (dired         dired-readin                 dirvish-setup-dired-buffer--advice)
    (dired         dired-get-subdir             dirvish-get-subdir--advice)
    (dired         dired-mark                   dirvish-update-line--advice)
    (dired         dired-flag-file-deletion     dirvish-update-line--advice)
    (dired         dired-goto-file              dirvish-update-line--advice)
    (dired         dired-internal-do-deletions  dirvish-deletion--advice)
    (dired         wdired-exit                  dirvish-refresh--advice)
    (dired         wdired-finish-edit           dirvish-refresh--advice)
    (dired         wdired-abort-changes         dirvish-refresh--advice)
    (dired-aux     dired-kill-line              dirvish-refresh--advice)
    (dired-aux     dired-do-kill-lines          dirvish-refresh--advice)
    (dired-aux     dired-create-directory       dirvish-refresh--advice)
    (dired-aux     dired-create-empty-file      dirvish-refresh--advice)
    (dired-aux     dired-do-create-files        dirvish-refresh--advice)
    (dired-aux     dired-insert-subdir          dirvish-refresh--advice)
    (dired-aux     dired-kill-subdir            dirvish-refresh--advice)
    (dired-aux     dired-rename-file            dirvish-revert--advice)
    (dired-narrow  dired-narrow--internal       dirvish-refresh--advice)
    (isearch       isearch-repeat-backward      dirvish-refresh--advice)
    (isearch       isearch-repeat-forward       dirvish-refresh--advice)
    (isearch       isearch-exit                 dirvish-refresh--advice)
    (find-dired    find-dired-sentinel          dirvish-refresh--advice)
    (evil          evil-refresh-cursor          dirvish-refresh-cursor--advice)
    (meow          meow--update-cursor          dirvish-refresh-cursor--advice)
    (lsp-mode      lsp-deferred                 ignore))
  "A list of file, adviced function, and advice function for `dirvish-add--advices'.")

(defun dirvish-add--advices ()
  "Add all advice listed in `dirvish-advice-alist'."
  (add-hook 'window-scroll-functions #'dirvish-update--viewports)
  (add-to-list 'display-buffer-alist
               '("\\(\\*info\\|\\*Help\\|\\*helpful\\|magit:\\).*"
                 (display-buffer-in-side-window)
                 (window-height . 0.4)
                 (side . bottom)))
  (add-function :after after-focus-change-function #'dirvish-redisplay--frame)
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (with-eval-after-load file (advice-add sym :around fn))))

(defun dirvish-clean--advices ()
  "Remove all advice listed in `dirvish-advice-alist'."
  (remove-hook 'window-scroll-functions #'dirvish-update--viewports)
  (setq display-buffer-alist (cdr display-buffer-alist))
  (remove-function after-focus-change-function #'dirvish-redisplay--frame)
  (pcase-dolist (`(,file ,sym ,fn) dirvish-advice-alist)
    (with-eval-after-load file (advice-remove sym fn))))

(put 'dired-subdir-alist 'permanent-local t)

;;;; Core setup

(defun dirvish-init (&optional one-window)
  "Save previous window config and initialize dirvish."
  (unless (or (posframe-workable-p) one-window)
    (user-error "dirvish.el: requires GUI."))
  (when (eq major-mode 'dirvish-mode) (dirvish-quit))
  (set-frame-parameter nil 'dirvish-one-window one-window)
  (when-let* ((ignore-one-win (not one-window))
              (frame (window-frame))
              (new-dirvish-frame (not (assoc frame dirvish-frame-alist))))
    (push (cons frame (current-window-configuration)) dirvish-frame-alist))
  (when (window-parameter nil 'window-side) (delete-window))
  (dirvish-init--buffer)
  (unless dirvish-initialized
    (dirvish-add--advices)
    (when dirvish-show-icons (setq dirvish-show-icons (ignore-errors (require 'all-the-icons))))
    (when (dirvish-get--i/o-status)
      (dirvish-repeat dirvish-update--footer 0 0.1)
      (dirvish-repeat dirvish-set--i/o-status 0 0.1))
    (when (featurep 'recentf) (setq dirvish-orig-recentf-list recentf-list))
    (mailcap-parse-mimetypes)
    (setq dirvish-initialized t)))

(defun dirvish-clean--buffers ()
  (cl-dolist (buf (buffer-list))
    (let ((name (buffer-name buf))
          (mode (buffer-local-value 'major-mode buf)))
      (when (or (eq 'dired-mode mode) (eq 'dirvish-mode mode)
                (and (not (string-equal name ""))
                     (string-match " \\*Dirvish .*" name)
                     (not (get-buffer-process buf))))
        (kill-buffer buf)))))

(defun dirvish-deinit ()
  "Revert previous window config and deinit dirvish."
  (setq dirvish-initialized nil)
  (setq recentf-list dirvish-orig-recentf-list)
  (mapc #'kill-buffer dirvish-preview-buffers)
  (let ((one-window (frame-parameter nil 'dirvish-one-window))
        (config (cdr-safe (assoc (window-frame) dirvish-frame-alist))))
    (if one-window
        (while (eq 'dirvish-mode (buffer-local-value 'major-mode (current-buffer)))
          (delq (selected-window) dirvish-parent-windows)
          (quit-window))
      (posframe-delete (frame-parameter nil 'dirvish-header-buffer))
      (set-frame-parameter nil 'dirvish-header--frame nil)
      (set-frame-parameter nil 'dirvish-preview-window nil)
      (setq dirvish-frame-alist (delq (assoc (window-frame) dirvish-frame-alist) dirvish-frame-alist))
      (when (window-configuration-p config)
        (set-window-configuration config)))
    (unless
        (or (and one-window (> (length dirvish-parent-windows) 1))
            (> (length dirvish-frame-alist) 1))
      (dirvish-clean--buffers)
      (dirvish-clean--advices)
      (cl-dolist (tm dirvish-repeat-timers) (cancel-timer (symbol-value tm))))
    (unless one-window (set-frame-parameter nil 'dirvish-one-window t))
    (setq dirvish-window nil)
    (setq dirvish-parent-windows ())
    (setq dirvish-preview-buffers ())
    (setq dirvish-parent-buffers ())))

(defun dirvish-refresh (&optional rebuild filter no-revert)
  "Reset dirvish. With optional prefix ARG (\\[universal-argument])
also rebuild dirvish layout."
  (interactive "P")
  (when rebuild
    (dirvish-build--parent-windows)
    (dirvish-build--preview-window)
    (dirvish-build--header-frame))
  (unless no-revert (revert-buffer))
  (when filter (dirvish-update--filter))
  (dirvish-update--padding)
  (dirvish-update--icons)
  (dirvish-update--line)
  (dirvish-update--preview)
  (dirvish-update--header)
  (dirvish-update--footer))

;;;; Core utilities

;;;###autoload
(defun dirvish (&optional path one-window)
  "Launch dired in dirvish-mode."
  (interactive)
  (let* ((file (or path buffer-file-name))
         (dir (if file (expand-file-name (file-name-directory file))
                (expand-file-name default-directory))))
    (dirvish-init one-window)
    (dirvish-find-file dir)))

(defun dirvish-find-file (&optional file ignore-hist)
  "Find file in dirvish buffer.

FILE can be a file or a directory, if nil then infer entry from
current `buffer-file-name'. If IGNORE-HIST is non-nil, do not
update `dirvish-history-ring'."
  (interactive)
  (let ((entry (or file (dired-get-filename nil t)))
        (bname (buffer-file-name (current-buffer)))
        (curr-dir (expand-file-name default-directory)))
    (when entry
      (if (file-directory-p entry)
          (let ((hist (directory-file-name entry)))
            (unless ignore-hist
              (when (or (ring-empty-p dirvish-history-ring)
                        (not (eq hist (ring-ref dirvish-history-ring 0))))
                (ring-insert dirvish-history-ring hist)))
            (switch-to-buffer (or (car (dired-buffers-for-dir entry))
                                  (dired-noselect entry)))
            (setq dirvish-child-entry (or bname curr-dir))
            (set-frame-parameter nil 'dirvish-index-path
                                 (or (dired-get-filename nil t) entry))
            (dirvish-refresh t))
        (find-file entry)))))

(defun dirvish-quit (&optional keep-alive)
  "Revert dirvish settings and disable dirvish."
  (interactive)
  (dirvish-deinit)
  (when (and (not keep-alive)
             (string= (frame-parameter nil 'name) "dirvish-emacs"))
    (delete-frame)))

(define-derived-mode dirvish-mode dired-mode "Dirvish"
  "Major mode emulating the dirvish file manager in `dired'."
  :group 'dirvish
  :interactive nil)

;;;###autoload
(defun dirvish-find-file-dwim (&rest args)
  "Call `dirvish-find-file' or `dired-find-file'."
  (if (derived-mode-p 'dirvish-mode)
      (apply 'dirvish-find-file args)
    (apply 'find-alternate-file args)))

;;;###autoload
(define-minor-mode dirvish-override-dired-mode
  "Setup dired buffer in a `dirvish' way."
  :group 'dirvish :global t
  (if dirvish-override-dired-mode
      (advice-add 'dired-jump :around #'dirvish-override-dired)
    (advice-remove 'dired-jump #'dirvish-override-dired)))

(provide 'dirvish)

;;; dirvish.el ends here
