;;; danger.el --- a Dired interface inspired by rANGER -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Package-Version: 0.6.3
;; Keywords: ranger, file, dired
;; Homepage: https://github.com/alexluigit/danger.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.0") (posframe "1.0.4"))

;;; Commentary:

;; danger.el is a minimalistic file manager based on `dired-mode'.  It
;; is inspired by ranger (see <https://github.com/ranger/ranger>)
;; which is a terminal file manager that shows a stack of the parent
;; directories and updates the parent buffers while navigating the
;; file system with an optional preview window at side showing the
;; content of the selected file. Unlike ranger.el, which tries to
;; become an all-around emulation of ranger (check their doc at
;; <https://github.com/ralesi/ranger.el>), danger.el is more bare-bone
;; in the sense that instead of trying to port all "goodness" from
;; ranger, it only provides a better dired UI and a few core features
;; relating to file management.

;;; Code:

(declare-function format-spec "format-spec")
(declare-function image-get-display-property "image-mode")
(require 'ring)
(require 'posframe)
(require 'dired-x)
(require 'ansi-color)
(require 'mailcap)
(eval-when-compile (require 'subr-x))

(defgroup danger nil
  "A better dired."
  :group 'dired)

;;; Variables

;;;; User facing options

(defcustom danger-show-hidden 'all
  "Determine hidden method in danger."
  :group 'danger
  :type '(radio (const :tag "Show All Files" :value 'all)
                (const :tag "Hide Common Files" :value 'danger)
                (const :tag "Hide All Dotfiles" :value 'dot)))

(defcustom danger-hidden-regexp
  '("^\\.\\(git\\|hg\\|svn\\)$"
    "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
    "~$" "^#.*#$")
  "Regexp of custom filetypes to omit in danger."
  :group 'danger :type 'list)

(defcustom danger-preview-cmd-alist
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
  :group 'danger :type '(alist :value-type ((choice list string) list)))

(defcustom danger-history-length 30
  "Length of history danger will track."
  :group 'danger :type 'integer)

(defcustom danger-cache-dir
  (concat (or (getenv "XDG_CACHE_HOME") (concat (getenv "HOME") "/.cache")) "/danger/")
  "Preview / thumbnail cache directory for danger."
  :group 'danger :type 'string)

(defcustom danger-depth 1
  "Number of directories up to traverse."
  :group 'danger :type 'integer)

(defcustom danger-enable-preview t
  "When not-nil preview the selected file."
  :group 'danger :type 'boolean)

(defcustom danger-width-parents 0.12
  "Fraction of frame width taken by parent windows."
  :group 'danger :type 'float)

(defcustom danger-max-parent-width 0.25
  "The max width allocated to showing parent windows."
  :group 'danger :type 'float)

(defcustom danger-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'danger :type 'float)

(defcustom danger-line-padding 0.1
  "doc"
  :group 'danger :type 'float)

(defcustom danger-header-string-fn 'danger--header-string
  "Function used to output a string that will show up as header."
  :group 'danger :type 'function)

(defcustom danger-header-position
  (lambda (_)
    (let ((tab-h (tab-bar-height nil t))
          (fringe (or (frame-parameter nil 'internal-border-width) 0)))
      (cons 0 (+ tab-h fringe))))
  "doc"
  :group 'danger :type 'function)

(defcustom danger-footer-format "Sort: %S  Filter: %f  %d  %p%w%t %i"
  "Format for footer display. "
  :group 'danger :type 'string)

(defcustom danger-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR) where DISK is path to a disk and
TRASH-DIR is path to trash-dir in that disk."
  :group 'danger :type 'alist)

(defcustom danger-show-icons t
  "When not-nil show file / dir icon."
  :group 'danger :type 'boolean)

(defcustom danger-icons-monochrome t
  "doc"
  :group 'danger :type 'boolean)

(defcustom danger-icons-v-offset 0.01
  "doc"
  :group 'danger :type 'float)

;;;; Faces

(defgroup danger-faces nil
  "Faces used by Danger."
  :group 'danger :group 'faces)

(defface danger-line-face
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Face used for `danger-update--line'."
  :group 'danger-faces)

;;;; Compiler

(defvar recentf-list)
(defvar danger-update--preview-timer)
(defvar posframe-mouse-banish)
(defvar image-mode-map)
(setq posframe-mouse-banish nil)

;;;; Internal variables

(defvar danger-width-img nil
  "Calculated preview window width. Used for image preview.")

(defvar danger-width-header nil
  "Calculated header frame width.")

(defvar danger-header-scale 1.25
  "Height of header line.")

(defvar danger-history-ring (make-ring danger-history-length)
  "History for `danger-find-file'.")

(defvar danger-initialized nil
  "Indicate if previous window config saved.")

(defvar danger-orig-recentf-list nil
  "doc")

(defvar danger-preview-delay 0.02
  "Time in seconds to delay running preview file functions.")

(defvar danger-footer-repeat 0.02
  "Time in seconds to repeat footer update.")

(defvar danger-sort-criteria ""
  "Default `ls' sorting switches.")

(defvar danger-window nil
  "Main danger window. Adjacent to preview window.")

(defvar danger-parent-windows ()
  "List of parent windows.")

(defvar danger-parent-buffers ()
  "List with buffers of parent buffers.")

(defvar danger-preview-buffers ()
  "List with buffers of previewed files.")

(defvar danger-frame-alist ()
  "List of frames using danger.")

(defvar danger-mode-hook nil
  "Hooks for setting danger parent windows.")

(defvar danger-preview-setup-hook nil
  "Hooks for setting danger preview windows.")

(defvar danger-repeat-timers '()
  "Timers with repeat flag need to be clean when exit.")

(defvar danger-i/o-queue ()
  "doc")

(defvar danger-override-dired-mode nil
  "doc")

;;;; Buffer / Frame local variables

(defvar-local danger-child-entry nil
  "doc")

(defvar danger-index-path nil
  "Latest path in danger-window.")

(defvar danger-preview-window nil
  "Window contains file / directory preview.")

(defvar danger-preview-buffer nil
  "doc")

(defvar danger-header-buffer nil
  "Buffer for showing header line.")

(defvar danger-header--frame nil
  "Frame for showing header line.")

;;;; Keymap

(defvar danger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB")                          'danger-show-history)
    (define-key map [remap dired-do-copy]                'danger-yank)
    (define-key map [remap dired-jump]                   'danger-jump)
    (define-key map [remap dired-do-redisplay]           'danger-change-level)
    (define-key map [remap dired-omit-mode]              'danger-toggle-dotfiles)
    (define-key map [remap dired-hide-details-mode]      'danger-toggle-preview)
    (define-key map [remap dired-find-file]              'danger-find-file)
    (define-key map [remap dired-find-alternate-file]    'danger-find-file)
    (define-key map [remap dired-up-directory]           'danger-up-directory)
    (define-key map [remap dired-next-line]              'danger-next-file)
    (define-key map [remap dired-previous-line]          'danger-prev-file)
    (define-key map [remap end-of-buffer]                'danger-go-bottom)
    (define-key map [remap beginning-of-buffer]          'danger-go-top)
    (define-key map [remap dired-sort-toggle-or-edit]    'danger-sort-by-criteria)
    (define-key map [remap revert-buffer]                'danger-refresh)
    (define-key map [remap dired-view-file]              'danger-toggle-preview)
    (define-key map [remap quit-window]                  'danger-quit)
    (define-key map [remap delete-window]
                (lambda ()
                  (interactive)
                  (message "%s" (substitute-command-keys "Press \\[quit-window] to quit danger"))))
    map)
  "Danger mode map.")

(with-eval-after-load 'image-mode
  (define-key image-mode-map [remap image-next-file] (lambda () (interactive) (danger) (danger-next-file 1)))
  (define-key image-mode-map [remap image-previous-file] (lambda () (interactive) (danger) (danger-next-file -1))))

;;; Layout

;;;; Header frame

(defun danger-width-header ()
  "Calculate header frame width. Default to frame width when disable preview."
  (* (frame-width) (if danger-enable-preview (- 1 danger-width-preview) 1)))

(defun danger-build--header-frame ()
  (let* ((buf (frame-parameter nil 'danger-header-buffer))
         (min-w (1+ (ceiling (danger-width-header))))
         (f-props `(:background-color
                    ,(face-attribute 'region :background)
                    :poshandler ,danger-header-position
                    :min-width ,min-w
                    :min-height 2))
         (h-frame (frame-parameter nil 'danger-header--frame))
         (size `(:posframe ,h-frame :height 2 :max-height 2 :min-height 2
                           :width: ,min-w :min-width ,min-w :max-width ,min-w)))
    (setq danger-width-header min-w)
    (if h-frame
        (posframe--set-frame-size size)
      (let ((fr (apply #'posframe-show buf f-props)))
        (set-frame-parameter nil 'danger-header--frame fr)))))

;;;; Parent windows

(defun danger-build--parent-windows ()
  (cl-flet ((danger-setup (child win buf)
              (when child (dired-goto-file child))
              (add-to-list 'danger-parent-windows win)
              (add-to-list 'danger-parent-buffers buf)
              (danger-mode)))
    (let* ((current (expand-file-name default-directory))
           (parent (danger-get--parent current))
           (parent-dirs ()) (i 0))
      (delete-other-windows)
      (setq danger-window (frame-selected-window))
      (danger-setup danger-child-entry danger-window (current-buffer))
      (when danger-enable-preview (dired-hide-details-mode t))
      (while (and (< i danger-depth) (not (string= current parent)))
        (setq i (+ i 1))
        (push (cons current parent) parent-dirs)
        (setq current (danger-get--parent current))
        (setq parent (danger-get--parent parent)))
      (let ((width (min (/ danger-max-parent-width danger-depth) danger-width-parents)))
        (cl-dolist (parent-dir parent-dirs)
          (let* ((current (car parent-dir))
                 (parent (cdr parent-dir))
                 (win-alist `((side . left)
                              (inhibit-same-window . t)
                              (window-width . ,width)))
                 (buffer (dired-noselect parent))
                 (window (display-buffer buffer `(danger-display--buffer . ,win-alist))))
            (with-selected-window window
              (danger-setup current window buffer)
              (dired-hide-details-mode t)
              (danger-update--padding)
              (danger-update--icons)
              (danger-update--line))))))))

;;;; Preview window

(defun danger-build--preview-window ()
  (when danger-enable-preview
    (let* ((inhibit-modification-hooks t)
           (win-alist `((side . right) (window-width . ,danger-width-preview)))
           (buf (frame-parameter nil 'danger-preview-buffer))
           (fringe 30)
           (new-window (display-buffer buf `(danger-display--buffer . ,win-alist))))
      (set-window-fringes new-window fringe fringe nil t)
      (setq danger-width-img (window-width new-window t))
      (set-frame-parameter nil 'danger-preview-window new-window))))

;;; Update

;;;; Preview

(defun danger--preview-process-sentinel (proc _exit)
  "doc"
  (let ((buf (frame-parameter nil 'danger-preview-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer (frame-parameter nil 'danger-preview-buffer)
        (erase-buffer) (remove-overlays)
        (let ((result-str (with-current-buffer (process-buffer proc) (buffer-string))))
          (insert result-str)
          (ansi-color-apply-on-region
           (point-min) (progn (goto-char (point-min)) (forward-line (frame-height)) (point))))))))

(cl-defun danger-get--preview-create (entry &optional cmd args)
  "Get corresponding preview buffer."
  (let ((buf (frame-parameter nil 'danger-preview-buffer))
        (process-connection-type nil)
        (size (number-to-string (or danger-minibuf-preview--width danger-width-img))) cache)
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (unless cmd (insert entry) (cl-return-from danger-get--preview-create buf))
      (when (string= cmd "*Preview Disable*")
        (insert (format "Preview Disabled for %s." entry))
        (cl-return-from danger-get--preview-create buf))
      (unless (executable-find cmd)
        (insert (format "Install `%s' to preview %s" cmd entry))
        (cl-return-from danger-get--preview-create buf))
      (when (or (member "%T" args) (member "%t" args)) (setq cache t))
      (cl-dolist (fmt `((,entry . "%i") (,size . "%s")))
        (setq args (cl-substitute (car fmt) (cdr fmt) args :test 'string=)))
      (unless cache
        (let* ((process-connection-type nil)
               (default-directory "~") ; Avoid "Setting current directory" error after deleting dir
               (res-buf (get-buffer-create " *Danger preview result*"))
               (proc (apply 'start-process "danger-preview-process" res-buf cmd args)))
          (with-current-buffer res-buf (erase-buffer) (remove-overlays))
          (set-process-sentinel proc 'danger--preview-process-sentinel))
        (cl-return-from danger-get--preview-create buf))
      (save-excursion
        ;; FIXME: a better way to deal with gif?
        (when (string= (mailcap-file-name-to-mime-type entry) "image/gif")
          (let ((gif-buf (find-file-noselect entry t nil))
                (callback (lambda (buf)
                            (with-current-buffer buf
                              (image-animate (image-get-display-property))))))
            (run-with-idle-timer 1 nil callback gif-buf)
            (cl-return-from danger-get--preview-create gif-buf)))
        (let* ((target-raw (concat danger-cache-dir size entry))
               (target-ext (concat target-raw ".jpg"))
               (target (if (and (string= cmd "convert")
                                (< (nth 7 (file-attributes entry)) (* 1024 1024 0.5)))
                           entry target-ext)))
          (if (file-exists-p target)
              (let ((img (create-image target nil nil :max-width danger-width-img)))
                (put-image img 0) (cl-return-from danger-get--preview-create buf))
            (make-directory (file-name-directory target-raw) t)
            (cl-dolist (format `((,target-raw . "%t") (,target-ext . "%T")))
              (setq args (cl-substitute (car format) (cdr format) args :test 'string=)))
            (let ((proc (apply 'start-process "danger-preview-process" buf cmd args)))
              (set-process-sentinel proc (lambda (_p _e) (danger-update--preview))))
            (insert "[Cache] Generating thumbnail..."))))
      buf)))

(cl-defun danger-match--preview-mime (file)
  "To determine if `FILE' can be matched by `danger-preview-cmd-alist'."
  (pcase-dolist (`(,re-or-exts ,cmd) danger-preview-cmd-alist)
    (if (listp re-or-exts)
        (let ((ext (file-name-extension file)))
          (when (member ext re-or-exts)
            (cl-return-from danger-match--preview-mime cmd)))
      (when (string-match re-or-exts (or (mailcap-file-name-to-mime-type file) ""))
        (cl-return-from danger-match--preview-mime cmd)))))

(cl-defun danger-preview--entry (entry)
  "Create the preview buffer of `ENTRY'."
  (unless (file-readable-p entry)
    (cl-return-from danger-preview--entry
      (danger-get--preview-create "File Not Readable")))
  (when (file-directory-p entry)
    (cl-return-from danger-preview--entry
      (danger-get--preview-create "directory" "exa" (list "--color=always" "-al" entry))))
  (let ((match (danger-match--preview-mime entry))
        (enable-local-variables nil)
        (inhibit-modification-hooks t)
        (auto-save-default nil)
        (delay-mode-hooks t)
        (inhibit-message t))
    (if match
        (let ((cmd (car match)) (args (cdr match)))
          (when (functionp cmd)
            (cl-return-from danger-preview--entry (apply cmd entry args)))
          (danger-get--preview-create entry cmd args))
      (let ((threshold (or large-file-warning-threshold 10000000))
            (filesize (file-attribute-size (file-attributes entry))))
        (if (> filesize threshold)
            (danger-get--preview-create (concat entry " too big for literal preview"))
          (find-file-noselect entry t nil))))))

(defun danger-update--preview (&optional preview-window)
  "Setup danger preview window."
  (when (or danger-enable-preview preview-window)
    (let* ((orig-buffer-list (buffer-list))
           (index (or (frame-parameter nil 'danger-index-path) ""))
           (preview-buffer (danger-preview--entry index))
           (preview-window (or preview-window (frame-parameter nil 'danger-preview-window))))
      (when (window-live-p preview-window)
        (set-window-buffer preview-window preview-buffer))
      (unless (memq preview-buffer orig-buffer-list)
        (push preview-buffer danger-preview-buffers))
      (with-current-buffer preview-buffer (run-hooks 'danger-preview-setup-hook)))))

;;;; Header / Footer / Filter

(defun danger-update--header ()
  "Update header string.  Make sure the length of header string
is less then `danger-width-header'."
  (with-current-buffer (frame-parameter nil 'danger-header-buffer)
    (erase-buffer)
    (let ((str (funcall danger-header-string-fn))
          (max-width (1- (floor (/ danger-width-header danger-header-scale)))))
      (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
        (setq str (substring str 0 -1)))
      (insert (concat str "\n")))
    (add-text-properties (point-min) (point-max)
                         `(display '(height ,danger-header-scale) line-spacing 0.5 line-height 1.5))))

(defun danger--header-string ()
  "Compose header string."
  (let* ((index (frame-parameter nil 'danger-index-path))
         (file-path (file-name-directory index))
         (path-prefix-home (string-prefix-p (getenv "HOME") file-path))
         (path-regex (concat (getenv "HOME") "/\\|\\/$"))
         (path-tail (replace-regexp-in-string path-regex "" file-path))
         (file-name (file-name-nondirectory index)))
    (format "   %s %s %s" (propertize (if path-prefix-home "~" ":"))
            (propertize path-tail 'face 'dired-mark)
            (propertize file-name 'face 'font-lock-constant-face))))

(defun danger-update--footer ()
  "Show file details in echo area."
  (when (and (dired-get-filename nil t) (danger-live-p))
    (let* ((fwidth (frame-width))
           (footer (format-spec danger-footer-format (danger--footer-spec)))
           (parts (split-string footer "&&&"))
           (lhs (nth 0 parts))
           (rhs (and (> (length parts) 1) (nth 1 parts)))
           (fringe-gap (if (eq fringe-mode 0) 4 2))
           (space (- fwidth fringe-gap (length lhs)))
           (message-log-max nil)
           (msg (format (format "%%s%%%ds" space) lhs rhs)))
      (message "%s" msg))))

(defun danger--footer-spec ()
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
         (sorting (if (string= "" danger-sort-criteria) "name" danger-sort-criteria))
         (i/o-task (or (danger-get--i/o-status) ""))
         (filter (format "%s" danger-show-hidden))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sorting) (?w . ,space) (?t . ,i/o-task))))

(defun danger-update--filter ()
  (save-excursion
    (let* ((all-re '("^\\.?#\\|^\\.$\\|^\\.\\.$"))
           (dot-re '("^\\."))
           (method (cl-case danger-show-hidden ('danger danger-hidden-regexp)
                            ('dot dot-re)))
           (omit-re (mapconcat 'concat (append all-re method) "\\|"))
           buffer-read-only)
      (dired-mark-unmarked-files omit-re nil nil 'no-dir)
      (goto-char (point-min))
      (let ((regexp (dired-marker-regexp)))
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))))

;;;; Overlays / Viewport

(defun danger-update--padding ()
  (save-excursion
    (remove-overlays)
    (let ((o (make-overlay (point-min) (point-max))))
      (setq line-spacing danger-line-padding)
      (overlay-put o 'display `(height ,(1+ danger-line-padding))))))

(defun danger-update--line ()
  (remove-overlays (point-min) (point-max) 'danger-line t)
  (when-let* ((pos (dired-move-to-filename nil))
              (beg (line-beginning-position))
              (end (line-beginning-position 2))
              (ol (make-overlay beg end)))
    (when danger-show-icons
      (remove-overlays beg end 'danger-icons t)
      (danger-render--icon pos 'danger-line-face))
    (overlay-put ol 'danger-line t)
    (overlay-put ol 'face 'danger-line-face)))

(defun danger-update--icons ()
  (when danger-show-icons
    (remove-overlays (point-min) (point-max) 'danger-icons t)
    (danger-update--attribute 'danger-render--icon)))

(defun danger-render--icon (pos &optional face)
  (let* ((entry (dired-get-filename 'relative 'noerror))
         (offset `(:v-adjust ,danger-icons-v-offset))
         (icon-face (or (when face `(:face ,face))
                        (when danger-icons-monochrome `(:face ,(face-at-point)))))
         (icon-attrs (append icon-face offset))
         (icon (if (file-directory-p entry)
                   (apply 'all-the-icons-icon-for-dir entry icon-attrs)
                 (apply 'all-the-icons-icon-for-file entry icon-attrs)))
         (icon-w/-offset (concat icon "\t"))
         (icon-str (propertize icon-w/-offset 'font-lock-face face))
         (ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'danger-icons t)
    (overlay-put ov 'after-string icon-str)))

(defun danger-update--attribute (render-func &optional range)
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

(defun danger-display--buffer (buffer alist)
  "Try displaying `BUFFER' at one side of the selected frame. This splits the
window at the designated `side' of the frame."
  (let* ((side (cdr (assq 'side alist)))
         (window-configuration-change-hook nil)
         (window-width (or (cdr (assq 'window-width alist)) 0.5))
         (size (ceiling (* (frame-width) window-width)))
         (split-width-threshold 0)
         (new-window (split-window-no-error danger-window size side)))
    (window--display-buffer buffer new-window 'window alist)))

(defun danger-get--parent (path)
  "Get parent directory of `PATH'"
  (file-name-directory (directory-file-name (expand-file-name path))))

(defun danger-get--filesize (fileset)
  "Determine file size of provided list of files in `FILESET'."
  (unless (executable-find "du") (user-error "`du' executable not found."))
  (with-temp-buffer
    (apply 'call-process "du" nil t nil "-sch" fileset)
    (format "%s" (progn (re-search-backward "\\(^[0-9.,]+[a-zA-Z]*\\).*total$")
                        (match-string 1)))))

(defun danger-get--trash-dir ()
  "Get trash directory for current disk."
  (cl-dolist (dir danger-trash-dir-alist)
    (when (string-prefix-p (car dir) (dired-current-directory))
      (cl-return (concat (car dir) (cdr dir))))))

(defun danger-get--i/o-status ()
  (when-let* ((task (car-safe danger-i/o-queue)))
    (let ((finished (car task))
          (size (nth 2 task))
          (index (car (nth 3 task)))
          (length (cdr (nth 3 task))))
      (when finished
        (setq danger-i/o-queue (cdr danger-i/o-queue))
        (unless danger-i/o-queue
          (cancel-timer (symbol-value 'danger-update--footer-timer))))
      (format "%s: %s total size: %s"
              (if finished "Success" "Progress")
              (propertize (format "%s / %s" index length) 'face 'font-lock-keyword-face)
              (propertize size 'face 'font-lock-builtin-face)))))

(defun danger-set--i/o-status ()
  (when-let* ((task (car-safe danger-i/o-queue)))
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
        (when (danger-live-p) (danger-refresh))
        (setf (nth 0 (car-safe danger-i/o-queue)) t)
        (when (eq (length danger-i/o-queue) 1)
          (cancel-timer (symbol-value 'danger-set--i/o-status-timer))))
      (setcar (nth 3 (car-safe danger-i/o-queue)) progress))))

(defmacro danger-repeat (func delay interval &rest args)
  "doc"
  (let ((timer (intern (format "%s-timer" func))))
    `(progn
       (defvar ,timer nil)
       (add-to-list 'danger-repeat-timers ',timer)
       (setq ,timer (run-with-timer ,delay ,interval ',func ,@args)))))

(defmacro danger-debounce (func delay &rest args)
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

(defun danger-jump (file)
  "Replacement for `dired-jump'"
  (interactive (list (read-file-name "Jump to: "))) (danger-find-file-dwim file))

(defun danger-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (danger-get--parent current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (let* ((output (shell-command-to-string "wmic logicaldisk get name"))
                 (drives (cdr (split-string output)))
                 (drive (completing-read "Select drive: " drives)))
            (when drive (danger-find-file drive))))
      (danger-find-file parent t))))

(defun danger-go-top ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-min)) (dired-next-line 1)
  (danger-next-file -1))

(defun danger-go-bottom ()
  "Move to bottom of file list"
  (interactive)
  (goto-char (point-max)) (danger-next-file 1))

(defun danger-next-file (arg)
  "Move lines in danger and initiate updates to preview window."
  (interactive "^p")
  (dired-next-line arg)
  (cond
   ((eobp) (unless (region-active-p) (forward-line -1)))
   ((bobp) (dired-next-line 1)))
  (when (dired-move-to-filename nil)
    (set-frame-parameter nil 'danger-index-path (dired-get-filename nil t))
    (danger-update--header)
    (danger-update--footer)
    (danger-debounce danger-update--preview danger-preview-delay)))

(defun danger-prev-file (arg)
  (interactive "^p")
  (danger-next-file (- 0 arg)))

(defun danger-show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list (completing-read "Select from history: "
                          (cl-remove-duplicates (ring-elements danger-history-ring)
                                                :test (lambda (x y) (or (null y) (equal x y)))))))
  (when history (danger-find-file history)))

;;;; Copy / Paste

(defun danger-paste (&optional mode)
  "doc"
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter 'buffer-live-p danger-parent-buffers))
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (error "No files marked for paste."))
    (danger-internal-paste yanked-files mode)))

(defun danger-internal-paste (fileset mode)
  "Helper for `danger-paste'."
  (let* ((target (dired-current-directory))
         (process-connection-type nil)
         (io-buffer (generate-new-buffer " *Danger I/O*"))
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
    (let ((size (danger-get--filesize (mapcar #'car new-fileset)))
          (leng (length new-fileset)))
      (add-to-list 'danger-i/o-queue `(nil ,io-buffer ,size ,(cons 0 leng) ,mode)))
    (danger-repeat danger-update--footer 0 0.1)
    (danger-repeat danger-set--i/o-status 0 0.1)
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf danger-parent-buffers)
      (with-current-buffer buf (dired-unmark-all-marks)))))

;;;; Utilities

(defun danger-yank (&optional arg)
  (interactive "P")
  (if arg (danger-paste 'move) (danger-paste)))

(defun danger-change-level (&optional arg)
  (interactive "p")
  (setq danger-depth (or arg 1)) (danger-refresh t))

(defun danger-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq danger-show-hidden
        (cl-case danger-show-hidden
          ('all 'dot) ('dot 'danger) ('danger 'all)))
  (danger-refresh nil t))

(defun danger-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq danger-enable-preview (not danger-enable-preview))
  (danger-refresh t))

(defun danger-sort-by-criteria (criteria)
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
      (setq danger-sort-criteria (car sort-flag))
      (dired-sort-other switch)
      (danger-refresh))))

;;;###autoload
(defun danger-live-p (&optional win)
  "Util function for detecting if in danger mode."
  (memq (or win (selected-window)) danger-parent-windows))

(defun danger-new-frame (&optional path)
  "Make a new frame and launch danger."
  (interactive)
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "danger-emacs") (alpha . (100 50))))))
    (with-selected-frame frame (danger path))))

;;; Init

(defun danger-init--buffer ()
  (let* ((index (number-to-string (length danger-frame-alist)))
         (header-buf (get-buffer-create (concat " *Danger Header-" index "*")))
         (preview-buf (get-buffer-create (concat " *Danger Preview-" index "*"))))
    (with-current-buffer preview-buf (setq mode-line-format nil))
    (with-current-buffer header-buf (setq-local face-font-rescale-alist nil))
    (set-frame-parameter nil 'danger-preview-buffer preview-buf)
    (set-frame-parameter nil 'danger-header-buffer header-buf)))

;;;; Advices / Hooks

(defun danger-redisplay--frame ()
  "Refresh danger frame, added to `after-focus-change-functions'."
  (if (eq major-mode 'danger-mode)
      (danger-refresh t)
    (when (memq (previous-frame) (mapcar 'car danger-frame-alist))
      (with-selected-frame (previous-frame)
        (danger-build--header-frame)
        (danger-update--header)))))

(defun danger-setup-dired-buffer--advice (fn &rest args)
  "Setup the dired buffer by removing the header and filter files."
  (apply fn args)
  (save-excursion
    (let ((inhibit-read-only t))
      (delete-region (point-min) (progn (forward-line 1) (point))))))

(defun danger-refresh--advice (fn &rest args)
  "Apply FN with ARGS, rebuild danger frame when necessary."
  (apply fn args)
  (let ((rebuild (not (eq major-mode 'danger-mode))))
    (danger-refresh rebuild nil 'no-revert)))

(defun danger-revert--advice (fn &rest args)
  "Apply FN with ARGS then revert buffer."
  (apply fn args) (danger-refresh))

(defun danger-refresh-cursor--advice (fn &rest args)
  (unless (and (not (eq major-mode 'wdired-mode)) (danger-live-p))
    (apply fn args)))

(defun danger-update-line--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (remove-overlays (point-min) (point-max) 'danger-line t)
  (when-let ((pos (dired-move-to-filename nil))
             danger-show-icons)
    (remove-overlays (1- pos) pos 'danger-icons t)
    (danger-render--icon pos))
  (apply fn args)
  (danger-update--line))

(defun danger-deletion--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (danger-get--trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (danger-next-file 1))
  (danger-refresh))

(defun danger-file-open--advice (fn &rest args)
  "Advice for commands that open a file."
  (when (danger-live-p) (danger-quit :keep-alive))
  (let ((default-directory "")) (apply fn args)))

(defun danger-other-window--advice (fn &rest args)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (danger-new-frame file)
      (apply fn args))))

(defun danger-update--viewports (win _)
  "Refresh attributes in viewport, added to `window-scroll-functions'."
  (when (and (eq win danger-window)
             (eq (selected-frame) (window-frame danger-window)))
    (with-selected-window win
      (danger-update--icons)
      (danger-update--line))))

(cl-dolist (fn '(danger-next-file danger-go-top danger-go-bottom danger-flag-file-yank))
  (advice-add fn :around 'danger-update-line--advice))

(with-eval-after-load 'pdf-tools
  (delete '(("pdf") ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" "%i" "%t"))
          danger-preview-cmd-alist))

(defvar danger-advice-alist
  '((files         find-file                    danger-file-open--advice)
    (files         find-file-other-window       danger-file-open--advice)
    (dired         dired-find-file-other-window danger-other-window--advice)
    (dired         dired-readin                 danger-setup-dired-buffer--advice)
    (dired         dired-mark                   danger-update-line--advice)
    (dired         dired-flag-file-deletion     danger-update-line--advice)
    (dired         dired-goto-file              danger-update-line--advice)
    (dired         dired-internal-do-deletions  danger-deletion--advice)
    (dired         wdired-exit                  danger-refresh--advice)
    (dired         wdired-finish-edit           danger-refresh--advice)
    (dired         wdired-abort-changes         danger-refresh--advice)
    (dired-aux     dired-kill-line              danger-refresh--advice)
    (dired-aux     dired-do-kill-lines          danger-refresh--advice)
    (dired-aux     dired-create-directory       danger-refresh--advice)
    (dired-aux     dired-create-empty-file      danger-refresh--advice)
    (dired-aux     dired-do-create-files        danger-refresh--advice)
    (dired-aux     dired-insert-subdir          danger-refresh--advice)
    (dired-aux     dired-kill-subdir            danger-refresh--advice)
    (dired-aux     dired-rename-file            danger-revert--advice)
    (dired-narrow  dired-narrow--internal       danger-refresh--advice)
    (isearch       isearch-repeat-backward      danger-refresh--advice)
    (isearch       isearch-repeat-forward       danger-refresh--advice)
    (isearch       isearch-exit                 danger-refresh--advice)
    (find-dired    find-dired-sentinel          danger-refresh--advice)
    (evil          evil-refresh-cursor          danger-refresh-cursor--advice)
    (meow          meow--update-cursor          danger-refresh-cursor--advice)
    (lsp-mode      lsp-deferred                 ignore))
  "A list of file, adviced function, and advice function.")

(defun danger-add--advices ()
  "Add all advice listed in `danger-advice-alist'."
  (pcase-dolist (`(,file ,sym ,fn) danger-advice-alist)
    (with-eval-after-load file (advice-add sym :around fn))))

(defun danger-clean--advices ()
  "Remove all advice listed in `danger-advice-alist'."
  (pcase-dolist (`(,file ,sym ,fn) danger-advice-alist)
    (with-eval-after-load file (advice-remove sym fn))))

(put 'dired-subdir-alist 'permanent-local t)

;;;; Core setup

(defun danger-init (&optional one-window)
  "Save previous window config and initialize danger."
  (set-frame-parameter nil 'danger-one-window one-window)
  (when-let* ((frame (window-frame))
              (new-danger-frame (not (assoc frame danger-frame-alist))))
    (push (cons frame (current-window-configuration)) danger-frame-alist))
  (when (window-parameter nil 'window-side) (delete-window))
  (danger-init--buffer)
  (unless danger-initialized
    (add-hook 'window-scroll-functions #'danger-update--viewports)
    (add-to-list 'display-buffer-alist
                 '("\\(\\*info\\|\\*Help\\|\\*helpful\\|magit:\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.4)
                   (side . bottom)))
    (add-function :after after-focus-change-function #'danger-redisplay--frame)
    (danger-add--advices)
    (unless (posframe-workable-p) (user-error "danger.el: requires GUI emacs."))
    (when danger-show-icons (setq danger-show-icons (ignore-errors (require 'all-the-icons))))
    (when (danger-get--i/o-status)
      (danger-repeat danger-update--footer 0 0.1)
      (danger-repeat danger-set--i/o-status 0 0.1))
    (when (featurep 'recentf) (setq danger-orig-recentf-list recentf-list))
    (mailcap-parse-mimetypes)
    (setq danger-initialized t)))

(defun danger-deinit ()
  "Revert previous window config and deinit danger."
  (setq danger-initialized nil)
  (setq recentf-list danger-orig-recentf-list)
  (when-let* ((config (cdr-safe (assoc (window-frame) danger-frame-alist)))
              (valid (window-configuration-p config)))
    (set-window-configuration config))
  (mapc #'kill-buffer danger-preview-buffers)
  (posframe-delete (frame-parameter nil 'danger-header-buffer))
  (set-frame-parameter nil 'danger-header--frame nil)
  (when-let ((singleton (< (length danger-frame-alist) 2)))
    (remove-hook 'window-scroll-functions #'danger-update--viewports)
    (setq display-buffer-alist (cdr display-buffer-alist))
    (remove-function after-focus-change-function #'danger-redisplay--frame)
    (danger-clean--advices)
    (cl-dolist (buf (buffer-list))
      (let ((name (buffer-name buf))
            (mode (buffer-local-value 'major-mode buf)))
        (when (or (eq 'dired-mode mode) (eq 'danger-mode mode)
                  (and (not (string-equal name ""))
                       (string-match " \\*Danger .*" name)
                       (not (get-buffer-process buf))))
          (kill-buffer buf))))
    (cl-dolist (tm danger-repeat-timers) (cancel-timer (symbol-value tm))))
  (set-frame-parameter nil 'danger-preview-window nil)
  (setq danger-frame-alist (delq (assoc (window-frame) danger-frame-alist) danger-frame-alist))
  (setq danger-window nil)
  (setq danger-parent-windows ())
  (setq danger-preview-buffers ())
  (setq danger-parent-buffers ()))

(defun danger-refresh (&optional rebuild filter no-revert)
  "Reset danger. With optional prefix ARG (\\[universal-argument])
also rebuild danger layout."
  (interactive "P")
  (when rebuild
    (danger-build--parent-windows)
    (danger-build--preview-window)
    (danger-build--header-frame))
  (unless no-revert (revert-buffer))
  (when filter (danger-update--filter))
  (danger-update--padding)
  (danger-update--icons)
  (danger-update--line)
  (danger-update--preview)
  (danger-update--header)
  (danger-update--footer))

;;;; Core utilities

;;;###autoload
(defun danger (&optional path)
  "Launch dired in danger-mode."
  (interactive)
  (let* ((file (or path buffer-file-name))
         (dir (if file (expand-file-name (file-name-directory file))
                (expand-file-name default-directory))))
    (danger-init)
    (danger-find-file dir)))

(defun danger-find-file (&optional file ignore-history)
  "Find file in danger buffer.  `ENTRY' can be used as path or filename, else will use
currently selected file in danger. `IGNORE-HISTORY' will not update history-ring on change"
  (interactive)
  (let ((entry (or file (dired-get-filename nil t)))
        (bname (buffer-file-name (current-buffer)))
        (curr-dir (expand-file-name default-directory)))
    (when entry
      (if (file-directory-p entry)
          (progn
            (unless ignore-history
              (when (or (ring-empty-p danger-history-ring)
                        (not (eq entry (ring-ref danger-history-ring 0))))
                (ring-insert danger-history-ring (directory-file-name entry))))
            (switch-to-buffer (or (car (dired-buffers-for-dir entry))
                                  (dired-noselect entry)))
            (setq danger-child-entry (or bname curr-dir))
            (set-frame-parameter nil 'danger-index-path
                                 (or (dired-get-filename nil t) entry))
            (danger-refresh t))
        (find-file entry)))))

(defun danger-quit (&optional keep-alive)
  "Revert danger settings and disable danger."
  (interactive)
  (danger-deinit)
  (when (and (not keep-alive)
             (string= (frame-parameter nil 'name) "danger-emacs"))
    (delete-frame)))

;;;###autoload
(define-minor-mode danger-override-dired-mode
  "Toggle danger to override dired whenever in danger-mode."
  :group 'danger :global t
  (let ((overrider '(lambda () (unless (danger-live-p) (danger)))))
    (if danger-override-dired-mode
        (add-hook 'dired-after-readin-hook overrider)
      (remove-hook 'dired-after-readin-hook overrider))))


(define-derived-mode danger-mode dired-mode "Danger"
  "Major mode emulating the danger file manager in `dired'."
  :group 'danger
  :interactive nil
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq tab-line-format nil)
  (setq header-line-format (propertize " " 'display
                                       `(height ,(* 2 (1+ danger-line-padding))))))
;;;###autoload
(defun danger-find-file-dwim (&rest args)
  "Call `danger-find-file' or `dired-find-file'."
  (if (derived-mode-p 'danger-mode)
      (apply 'danger-find-file args)
    (apply 'find-alternate-file args)))


(provide 'danger)

;;; danger.el ends here
