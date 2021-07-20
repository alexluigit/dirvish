;;; lf.el --- A modern file manager based on dired -*- lexical-binding: t -*-
;; Copyright (C) 2021 Alex Lu

;; Author : Alex Lu <https://github.com/alexluigit>
;; Package-Version: 0.6.3
;; Keywords: lf, files, dired
;; Homepage: https://github.com/alexluigit/lf.el
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (posframe "1.0.4") (async "1.9.5") (all-the-icons "5.0.0"))

;;; Commentary:

;; lf stands for "list files", it is a minimalistic file manager based on
;; `dired-mode'.  It is inspired by lf, see https://github.com/gokcehan/lf. A
;; similar project is ranger.el, see https://github.com/ralesi/ranger.el.  It
;; shows a stack of the parent directories and updates the parent buffers while
;; navigating the file system with an optional preview window at side showing
;; the content or meta information of the selected file. Unlike ranger or
;; ranger.el, lf is more bare-bone and only focus on core functionalities
;; involving file management.

;;; Code:

(declare-function format-spec "format-spec")
(declare-function image-get-display-property "image-mode")
(declare-function selectrum--get-candidate "selectrum")
(declare-function selectrum--get-full "selectrum")
(declare-function vertico--candidate "vertico")
(require 'ring)
(require 'transient)
(require 'posframe)
(require 'dired-x)
(require 'all-the-icons)
(require 'ansi-color)
(require 'mailcap)
(require 'async)
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
  '(("text/"             (find-file-noselect . (t nil)))
    ("image/"            ("convert" "-resize" "%s" "%i" "%T"))
    ("audio/"            ("mediainfo" "%i"))
    ("video/"            ("ffmpegthumbnailer" "-i" "%i" "-o" "%T" "-s 0"))
    (("iso" "bin" "exe") ("*Preview Disable*"))
    (("zip")             ("zipinfo" "%i"))
    (("zst" "tar")       ("tar" "-tvf" "%i"))
    (("epub")            ("epub-thumbnailer" "%i" "%T" "1024"))
    (("pdf")             ("pdftoppm" "-jpeg" "-f" "1" "-singlefile" "%i" "%t")))
  "doc"
  :group 'lf :type '(alist :value-type ((choice list string) list)))

(defcustom lf-routes '(("h" "home" "~") ("u" "media" "/media"))
  "doc"
  :group 'lf :type 'list
  :set
  (lambda (k v)
    `(setq ,k v)
    (eval `(transient-define-prefix lf-routes ()
             ["Go to Directory: "
              ,@(cl-loop for (key desc path) in v
                         collect (list key desc `(lambda () (interactive) (lf-find-file ,path))))]))))

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

(defcustom lf-width-parents 0.12
  "Fraction of frame width taken by parent windows."
  :group 'lf :type 'float)

(defcustom lf-max-parent-width 0.25
  "The max width allocated to showing parent windows."
  :group 'lf :type 'float)

(defcustom lf-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'lf :type 'float)

(defcustom lf-line-padding 0.1
  "doc"
  :group 'lf :type 'float)

(defcustom lf-header-string-fn 'lf--header-string
  "Function used to output a string that will show up as header."
  :group 'lf :type 'function)

(defcustom lf-header-position
  (lambda (_) (cons 0 (or (frame-parameter nil 'internal-border-width) 0)))
  "doc"
  :group 'lf :type 'function)

(defcustom lf-minibuf-preview-position
  (lambda (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width)) 2)
          (or (* (frame-parameter nil 'internal-border-width) 2) 60)))
  "doc"
  :group 'lf :type 'function)

(defcustom lf-footer-format "Sort: %S  Filter: %f  %d  %p%w%t %i"
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

;;;; Compiler

(defvar recentf-list)
(defvar selectrum--current-candidate-index)
(defvar lf-update--preview-timer)

;;;; Internal variables

(defvar lf-width-img nil
  "Calculated preview window width. Used for image preview.")

(defvar lf-width-header nil
  "Calculated header frame width.")

(defvar lf-header-scale 1.25
  "Height of header line.")

(defvar lf-history-ring (make-ring lf-history-length)
  "History for `lf-find-file'.")

(defvar lf-initialized nil
  "Indicate if previous window config saved.")

(defvar lf-orig-recentf-list nil
  "doc")

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

(defvar lf-mode-hook nil
  "Hooks for setting lf parent windows.")

(defvar lf-preview-setup-hook nil
  "Hooks for setting lf preview windows.")

(defvar lf-repeat-timers '()
  "Timers with repeat flag need to be clean when exit.")

(defvar lf-i/o-queue ()
  "doc")

(defvar lf-override-dired-mode nil
  "doc")

(defvar lf-minibuf-preview-categories '(file project-file)
  "doc")

(defvar lf-minibuf-preview--height (- 1 (* max-mini-window-height 1.5))
  "doc")

(defvar lf-minibuf-preview--width nil
  "doc")

(defvar lf-minibuf-preview-window nil
  "doc")

(defvar lf-minibuf-preview--category nil
  "doc")

(defvar lf-minibuf--get-candidate
  (cond ((bound-and-true-p vertico-mode)
         (lambda () (vertico--candidate)))
        ((bound-and-true-p selectrum-mode)
         (lambda ()
           (selectrum--get-full
            (selectrum--get-candidate selectrum--current-candidate-index)))))
  "doc")

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

;;;; Keymap

(transient-define-prefix lf-file ()
  "Get file information."
  ["Select file operation:"
   ("m" "mark file REGEX" dired-mark-files-regexp)
   ("n" "copy file NAME" dired-copy-filename-as-kill)
   ("p" "copy file PATH" lf-yank-filepath)
   ("t" "show file TYPE" dired-show-file-type)
   ("l" "goto file TRUEPATH" lf-file-truename)])

(defvar lf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f"                                  'lf-file)
    (define-key map "y"                                  'lf-yank)
    (define-key map "r"                                  'lf-routes)
    (define-key map (kbd "TAB")                          'lf-show-history)
    (define-key map [remap dired-jump]                   'lf-jump)
    (define-key map [remap dired-do-redisplay]           'lf-change-level)
    (define-key map [remap dired-omit-mode]              'lf-toggle-dotfiles)
    (define-key map [remap dired-hide-details-mode]      'lf-toggle-preview)
    (define-key map [remap dired-find-file]              'lf-find-file)
    (define-key map [remap dired-up-directory]           'lf-up-directory)
    (define-key map [remap dired-next-line]              'lf-next-file)
    (define-key map [remap dired-previous-line]          'lf-prev-file)
    (define-key map [remap end-of-buffer]                'lf-go-bottom)
    (define-key map [remap beginning-of-buffer]          'lf-go-top)
    (define-key map [remap dired-sort-toggle-or-edit]    'lf-sort-criteria)
    (define-key map [remap revert-buffer]                'lf-refresh)
    (define-key map [remap dired-view-file]              'lf-toggle-preview)
    (define-key map [remap quit-window]                  'lf-quit)
    (define-key map [remap delete-window]
      (lambda ()
        (interactive)
        (message "%s" (substitute-command-keys "Press \\[quit-window] to quit lf"))))
    map)
  "Lf mode map.")

;;; Layout

;;;; Header frame

(defun lf-width-header ()
  "Calculate header frame width. Default to frame width when disable preview."
  (* (frame-width) (if lf-enable-preview (- 1 lf-width-preview) 1)))

(defun lf-build--header-frame ()
  (let* ((buf (frame-parameter nil 'lf-header-buffer))
         (min-w (1+ (ceiling (lf-width-header))))
         (f-props `(:background-color
                    ,(face-attribute 'region :background)
                    :poshandler ,lf-header-position
                    :min-width ,min-w
                    :min-height 2))
         (h-frame (frame-parameter nil 'lf-header--frame)))
    (setq lf-width-header min-w)
    (if h-frame
        (posframe--set-frame-size h-frame 1 2 1 min-w)
      (set-frame-parameter nil 'lf-header--frame (apply #'posframe-show buf f-props)))))

;;;; Parent windows

(defun lf-build--parent-windows ()
  (cl-flet ((lf-setup (child win buf)
                      (when child (dired-goto-file child))
                      (add-to-list 'lf-parent-windows win)
                      (add-to-list 'lf-parent-buffers buf)
                      (lf-mode)))
    (let* ((current (expand-file-name default-directory))
           (parent (lf-get--parent current))
           (parent-dirs ()) (i 0))
      (delete-other-windows)
      (setq lf-window (frame-selected-window))
      (lf-setup lf-child-entry lf-window (current-buffer))
      (when lf-enable-preview (dired-hide-details-mode t))
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
            (with-selected-window window
              (lf-setup current window buffer)
              (dired-hide-details-mode t)
              (lf-update--padding)
              (lf-update--icons)
              (lf-update--line))))))))

;;;; Preview window

(defun lf-build--preview-window ()
  (when lf-enable-preview
    (let* ((inhibit-modification-hooks t)
           (win-alist `((side . right) (window-width . ,lf-width-preview)))
           (buf (frame-parameter nil 'lf-preview-buffer))
           (fringe 30)
           (new-window (display-buffer buf `(lf-display--buffer . ,win-alist))))
      (set-window-fringes new-window fringe fringe nil t)
      (setq lf-width-img (window-width new-window t))
      (set-frame-parameter nil 'lf-preview-window new-window))))

;;;; Minibuf preview frame

(defun lf-minibuf-preview-create ()
  "doc"
  (when-let* ((meta (completion-metadata
                     (buffer-substring-no-properties (field-beginning) (point))
                     minibuffer-completion-table
                     minibuffer-completion-predicate))
              (category (completion-metadata-get meta 'category))
              (show-preview (memq category lf-minibuf-preview-categories)))
    (with-eval-after-load 'lsp-mode (advice-add 'lsp-deferred :around #'ignore))
    (setq lf-minibuf-preview--category category)
    (walk-window-tree (lambda (w)
                        (when (window-live-p w)
                          (with-current-buffer (window-buffer w)
                            (let ((ov (make-overlay (point-min) (point-max))))
                              (overlay-put ov 'temp-inactive-ov t)
                              (overlay-put ov 'font-lock-face 'font-lock-doc-face))))))
    (setq lf-minibuf-preview-window (frame-parameter nil 'lf-preview-window))
    (unless lf-minibuf-preview-window
      (let* ((min-w (ceiling (* (frame-width) lf-width-preview)))
             (min-h (ceiling (* (frame-height) lf-minibuf-preview--height)))
             (b-color (face-attribute 'font-lock-doc-face :foreground))
             (pos-f (or lf-minibuf-preview-position #'posframe-poshandler-frame-top-center))
             (override `((minibuffer . ,(active-minibuffer-window))))
             (f-props `(:min-width ,min-w :min-height ,min-h :poshandler ,pos-f
                                   :override-parameters ,override
                                   :border-width 5 :border-color ,b-color))
             (frame (apply #'posframe-show "*candidate preview*" f-props)))
        (setq lf-minibuf-preview-window (frame-root-window frame))
        (set-window-fringes lf-minibuf-preview-window 30 30 nil t)))
    (lf-init--buffer)
    (setq lf-minibuf-preview--width (window-width lf-minibuf-preview-window t))
    (set-window-dedicated-p lf-minibuf-preview-window nil)))

(defun lf-minibuf-preview-teardown ()
  "doc"
  (posframe-delete "*candidate preview*")
  (with-eval-after-load 'lsp-mode (advice-remove 'lsp-deferred #'ignore))
  (walk-window-tree (lambda (w)
                      (with-current-buffer (window-buffer w)
                        (remove-overlays (point-min) (point-max) 'temp-inactive-ov t))))
  (setq lf-minibuf-preview--category nil)
  (setq lf-minibuf-preview--width nil)
  (mapc 'kill-buffer lf-preview-buffers))

;;; Update

;;;; Preview

(cl-defun lf-get--preview-create (entry &optional cmd args)
  "Get corresponding preview buffer."
  (let ((buf (frame-parameter nil 'lf-preview-buffer))
        (process-connection-type nil)
        (size (number-to-string (or lf-minibuf-preview--width lf-width-img))) cache)
    (with-current-buffer buf
      (erase-buffer) (remove-overlays)
      (unless cmd (insert entry) (cl-return-from lf-get--preview-create buf))
      (when (string= cmd "*Preview Disable*")
        (insert (format "Preview Disabled for %s." entry))
        (cl-return-from lf-get--preview-create buf))
      (unless (executable-find cmd)
        (insert (format "Install `%s' to preview %s" cmd entry))
        (cl-return-from lf-get--preview-create buf))
      (save-excursion
        (when (or (member "%T" args) (member "%t" args)) (setq cache t))
        (cl-dolist (fmt `((,entry . "%i") (,size . "%s")))
          (setq args (cl-substitute (car fmt) (cdr fmt) args :test 'string=)))
        (unless cache
          (let ((default-directory "~")) ; Avoid "Setting current directory" error after deleting dir
            (apply #'call-process cmd nil t nil args))
          (when (string= cmd "exa")
            (ansi-color-apply-on-region
             (point-min) (progn (goto-char (point-min)) (forward-line (frame-height)) (point))))
          (cl-return-from lf-get--preview-create buf))
        ;; FIXME: a better way to deal with gif?
        (when (string= (mailcap-file-name-to-mime-type entry) "image/gif")
          (let ((gif-buf (find-file-noselect entry t nil))
                (callback (lambda (buf)
                            (with-current-buffer buf
                              (image-animate (image-get-display-property))))))
            (run-with-idle-timer 1 nil callback gif-buf)
            (cl-return-from lf-get--preview-create gif-buf)))
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
            (let ((callback (lambda (res) (ignore res) (lf-update--preview))))
              (apply #'async-start-process (append (list "Lf I/O" cmd callback) args)))
            (insert "[Cache] Generating thumbnail..."))))
      buf)))

(cl-defun lf-match--preview-mime (file)
  "To determine if `FILE' can be matched by `lf-preview-cmd-alist'."
  (pcase-dolist (`(,re-or-exts ,cmd) lf-preview-cmd-alist)
    (if (listp re-or-exts)
        (let ((ext (file-name-extension file)))
          (when (member ext re-or-exts)
            (cl-return-from lf-match--preview-mime cmd)))
      (when (string-match re-or-exts (or (mailcap-file-name-to-mime-type file) ""))
        (cl-return-from lf-match--preview-mime cmd)))))

(cl-defun lf-preview--entry (entry)
  "Create the preview buffer of `ENTRY'."
  (unless (file-readable-p entry)
    (cl-return-from lf-preview--entry
      (lf-get--preview-create "File Not Readable")))
  (when (file-directory-p entry)
    (cl-return-from lf-preview--entry
      (lf-get--preview-create "directory" "exa" (list "--color=always" "-al" entry))))
  (let ((match (lf-match--preview-mime entry))
        (inhibit-modification-hooks t)
        (auto-save-default nil)
        (delay-mode-hooks t)
        (inhibit-message t))
    (if match
        (let ((cmd (car match)) (args (cdr match)))
          (when (functionp cmd)
            (cl-return-from lf-preview--entry (apply cmd entry args)))
          (lf-get--preview-create entry cmd args))
      (let ((threshold (or large-file-warning-threshold 10000000))
            (filesize (file-attribute-size (file-attributes entry))))
        (if (> filesize threshold)
            (lf-get--preview-create (concat entry " too big for literal preview"))
          (find-file-noselect entry t nil))))))

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
      (with-current-buffer preview-buffer (run-hooks 'lf-preview-setup-hook)))))

;;;; Header / Footer / Filter

(defun lf-update--header ()
  "Update header string.  Make sure the length of header string
is less then `lf-width-header'."
  (with-current-buffer (frame-parameter nil 'lf-header-buffer)
    (erase-buffer)
    (let ((str (funcall lf-header-string-fn))
          (max-width (1- (floor (/ lf-width-header lf-header-scale)))))
      (while (>= (+ (length str) (/ (- (string-bytes str) (length str)) 2)) max-width)
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
         (i/o-task (or (lf-get--i/o-status) ""))
         (filter (format "%s" lf-show-hidden))
         (space "&&&"))
    `((?u . ,user) (?d . ,file-date) (?p . ,file-perm) (?i . ,index) (?f . ,filter)
      (?s . ,file-size) (?S . ,sorting) (?w . ,space) (?t . ,i/o-task))))

(defun lf-update--filter ()
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

;;;; Overlays / Viewport

(defun lf-update--padding ()
  (save-excursion
    (remove-overlays)
    (let ((o (make-overlay (point-min) (point-max))))
      (setq line-spacing lf-line-padding)
      (overlay-put o 'display `(height ,(1+ lf-line-padding))))))

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

(defun lf-update--icons ()
  (when lf-show-icons
    (remove-overlays (point-min) (point-max) 'lf-icons t)
    (lf-update--attribute 'lf-render--icon)))

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

(defun lf-update--attribute (render-func &optional range)
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
  (unless (executable-find "du") (user-error "`du' executable not found."))
  (with-temp-buffer
    (apply 'call-process "du" nil t nil "-sch" fileset)
    (format "%s" (progn (re-search-backward "\\(^[0-9.,]+[a-zA-Z]*\\).*total$")
                        (match-string 1)))))

(defun lf-get--trash-dir ()
  "Get trash directory for current disk."
  (cl-dolist (dir lf-trash-dir-alist)
    (when (string-prefix-p (car dir) (dired-current-directory))
      (cl-return (concat (car dir) (cdr dir))))))

(defun lf-get--i/o-status ()
  (when-let* ((task (car-safe lf-i/o-queue)))
    (let ((finished (car task))
          (size (nth 2 task))
          (index (car (nth 3 task)))
          (length (cdr (nth 3 task))))
      (when finished
        (setq lf-i/o-queue (cdr lf-i/o-queue))
        (unless lf-i/o-queue
          (cancel-timer (symbol-value 'lf-update--footer-timer))))
      (format "%s: %s total size: %s"
              (if finished "Success" "Progress")
              (propertize (format "%s / %s" index length) 'face 'font-lock-keyword-face)
              (propertize size 'face 'font-lock-builtin-face)))))

(defun lf-set--i/o-status ()
  (when-let* ((task (car-safe lf-i/o-queue)))
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
        (when (lf-live-p) (lf-refresh))
        (setf (nth 0 (car-safe lf-i/o-queue)) t)
        (when (eq (length lf-i/o-queue) 1)
          (cancel-timer (symbol-value 'lf-set--i/o-status-timer))))
      (setcar (nth 3 (car-safe lf-i/o-queue)) progress))))

(defmacro lf-delay--repeat (func delay interval &rest args)
  "doc"
  (let ((timer (intern (format "%s-timer" func))))
    `(progn
       (defvar ,timer nil)
       (add-to-list 'lf-repeat-timers ',timer)
       (setq ,timer (run-with-timer ,delay ,interval ',func ,@args)))))

(defmacro lf-delay--once (func delay &rest args)
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

(defun lf-jump (file)
  "Replacement for `dired-jump'"
  (interactive (list (read-file-name "Jump to: "))) (lf file))

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
    (lf-delay--once lf-update--preview lf-preview-delay)))

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

;;;; Copy / Paste

(defun lf-paste (&optional mode)
  "doc"
  (interactive)
  (let* ((regexp (dired-marker-regexp))
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
    (let ((size (lf-get--filesize (mapcar #'car new-fileset)))
          (leng (length new-fileset)))
      (add-to-list 'lf-i/o-queue `(nil ,io-buffer ,size ,(cons 0 leng) ,mode)))
    (lf-delay--repeat lf-update--footer 0 0.1)
    (lf-delay--repeat lf-set--i/o-status 0 0.1)
    (cl-dolist (file new-fileset)
      (funcall paste-func (car file) (cdr file)))
    (cl-dolist (buf lf-parent-buffers)
      (with-current-buffer buf (dired-unmark-all-marks)))))

;;;; Utilities

(defun lf-file-truename ()
  (interactive)
  (lf-find-file (file-truename (dired-get-filename))))

(defun lf-yank (&optional arg)
  (interactive "P")
  (if arg (lf-paste 'move) (lf-paste)))

(defun lf-change-level (&optional arg)
  (interactive "p")
  (setq lf-depth (or arg 1)) (lf-refresh t))

(defun lf-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq lf-show-hidden
        (cl-case lf-show-hidden
          ('all 'dot) ('dot 'lf) ('lf 'all)))
  (lf-refresh nil t))

(defun lf-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq lf-enable-preview (not lf-enable-preview))
  (lf-refresh t))

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

(defun lf-yank-filepath (&optional dir-only)
  "Copy the current directory's (`default-directory''s) absolute path."
  (interactive "P")
  (if dir-only
      (message (kill-new (expand-file-name default-directory)))
    (message (kill-new (dired-get-filename nil t)))))

;;;###autoload
(defun lf-live-p (&optional win)
  "Util function for detecting if in lf mode."
  (memq (or win (selected-window)) lf-parent-windows))

(defun lf-new-frame (&optional path)
  "Make a new frame and launch lf."
  (interactive)
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "lf-emacs") (alpha . (100 50))))))
    (with-selected-frame frame (lf path))))

;;; Init

(defun lf-init--buffer ()
  (let* ((index (number-to-string (length lf-frame-alist)))
         (header-buf (get-buffer-create (concat "*Lf Header-" index "*")))
         (preview-buf (get-buffer-create (concat "*Lf Preview-" index "*"))))
    (with-current-buffer preview-buf (setq mode-line-format nil))
    (set-frame-parameter nil 'lf-preview-buffer preview-buf)
    (set-frame-parameter nil 'lf-header-buffer header-buf)))

;;;; Advices / Hooks

(defun lf-redisplay--frame ()
  "Refresh lf frame, added to `after-focus-change-functions'."
  (if (eq major-mode 'lf-mode)
      (lf-refresh t)
    (when (memq (previous-frame) (mapcar 'car lf-frame-alist))
      (with-selected-frame (previous-frame)
        (lf-build--header-frame)
        (lf-update--header)))))

(defun lf-setup-dired-buffer--advice (fn &rest args)
  "Setup the dired buffer by removing the header and filter files."
  (apply fn args)
  (save-excursion
    (let ((inhibit-read-only t))
      (delete-region (point-min) (progn (forward-line 1) (point))))))

(defun lf-refresh--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (apply fn args)
  (let ((rebuild (not (eq major-mode 'lf-mode))))
    (lf-refresh rebuild nil t)))

(defun lf-refresh-cursor--advice (fn &rest args)
  (unless (and (not (eq major-mode 'wdired-mode)) (lf-live-p))
    (apply fn args)))

(defun lf-update-line--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (remove-overlays (point-min) (point-max) 'lf-line t)
  (when-let ((pos (dired-move-to-filename nil))
             lf-show-icons)
    (remove-overlays (1- pos) pos 'lf-icons t)
    (lf-render--icon pos))
  (apply fn args)
  (lf-update--line))

(defun lf-deletion--advice (fn &rest args)
  "Advice function for FN with ARGS."
  (let ((trash-directory (lf-get--trash-dir))) (apply fn args))
  (unless (dired-get-filename nil t) (lf-next-file 1))
  (lf-refresh))

(defun lf-file-open--advice (fn &rest args)
  "Advice for `find-file' and `find-file-other-window'"
  (when (lf-live-p) (lf-quit :keep-alive)) (apply fn args))

(defun lf-other-window--advice (fn &rest args)
  (let ((file (dired-get-file-for-visit)))
       (if (file-directory-p file)
           (lf-new-frame file)
         (apply fn args))))

(defun lf-minibuf--update-advice (fn &rest args)
  "doc"
  (apply fn args)
  (when-let* ((category lf-minibuf-preview--category)
              (cand (funcall lf-minibuf--get-candidate)))
    (if (eq category 'project-file)
        (setq cand (expand-file-name cand (or (cdr-safe (project-current))
                                              (car (minibuffer-history-value)))))
      (setq cand (expand-file-name cand)))
    (set-frame-parameter nil 'lf-index-path cand)
    (lf-delay--once lf-update--preview lf-preview-delay lf-minibuf-preview-window)))

(defun lf-update--viewports (win _)
  "Refresh attributes in viewport, added to `window-scroll-functions'."
  (when (and (eq win lf-window)
             (eq (selected-frame) (window-frame lf-window)))
    (with-selected-window win
      (lf-update--icons)
      (lf-update--line))))

(cl-dolist (fn '(lf-next-file lf-go-top lf-go-bottom lf-flag-file-yank))
  (advice-add fn :around 'lf-update-line--advice))

(defvar lf-advice-alist
  '((files         find-file                    lf-file-open--advice)
    (files         find-file-other-window       lf-file-open--advice)
    (dired         dired-find-file-other-window lf-other-window--advice)
    (dired         dired-readin                 lf-setup-dired-buffer--advice)
    (dired         dired-mark                   lf-update-line--advice)
    (dired         dired-flag-file-deletion     lf-update-line--advice)
    (dired         dired-goto-file              lf-update-line--advice)
    (dired         dired-internal-do-deletions  lf-deletion--advice)
    (dired         wdired-exit                  lf-refresh--advice)
    (dired         wdired-finish-edit           lf-refresh--advice)
    (dired         wdired-abort-changes         lf-refresh--advice)
    (dired-aux     dired-do-kill-lines          lf-refresh--advice)
    (dired-aux     dired-create-directory       lf-refresh--advice)
    (dired-aux     dired-create-empty-file      lf-refresh--advice)
    (dired-aux     dired-do-create-files        lf-refresh--advice)
    (dired-aux     dired-do-rename              lf-refresh--advice)
    (dired-aux     dired-insert-subdir          lf-refresh--advice)
    (dired-narrow  dired-narrow--internal       lf-refresh--advice)
    (isearch       isearch-repeat-backward      lf-refresh--advice)
    (isearch       isearch-repeat-forward       lf-refresh--advice)
    (isearch       isearch-exit                 lf-refresh--advice)
    (find-dired    find-dired-sentinel          lf-refresh--advice)
    (evil          evil-refresh-cursor          lf-refresh-cursor--advice)
    (meow          meow--update-cursor          lf-refresh-cursor--advice)
    (lsp-mode      lsp-deferred                 ignore))
  "A list of file, adviced function, and advice function.")

(put 'dired-subdir-alist 'permanent-local t)

;;;; Core setup

(defun lf-init ()
  "Save previous window config and initialize lf."
  (when-let* ((frame (window-frame))
              (new-lf-frame (not (assoc frame lf-frame-alist))))
    (push (cons frame (current-window-configuration)) lf-frame-alist))
  (when (window-parameter nil 'window-side) (delete-window))
  (lf-init--buffer)
  (unless lf-initialized
    (add-hook 'window-scroll-functions #'lf-update--viewports)
    (add-to-list 'display-buffer-alist
                 '("\\(\\*info\\|\\*Help\\|\\*helpful\\|magit:\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.4)
                   (side . bottom)))
    (add-function :after after-focus-change-function #'lf-redisplay--frame)
    (pcase-dolist (`(,file ,sym ,fn) lf-advice-alist)
      (with-eval-after-load file (advice-add sym :around fn)))
    (unless (posframe-workable-p) (error "Lf requires GUI emacs."))
    (when (lf-get--i/o-status)
      (lf-delay--repeat lf-update--footer 0 0.1)
      (lf-delay--repeat lf-set--i/o-status 0 0.1))
    (setq lf-orig-recentf-list recentf-list)
    (mailcap-parse-mimetypes)
    (setq lf-initialized t)))

(defun lf-deinit ()
  "Revert previous window config and deinit lf."
  (setq lf-initialized nil)
  (setq recentf-list lf-orig-recentf-list)
  (when-let* ((config (cdr-safe (assoc (window-frame) lf-frame-alist)))
              (valid (window-configuration-p config)))
    (set-window-configuration config))
  (mapc #'kill-buffer lf-preview-buffers)
  (posframe-delete (frame-parameter nil 'lf-header-buffer))
  (set-frame-parameter nil 'lf-header--frame nil)
  (when-let ((singleton (< (length lf-frame-alist) 2)))
    (remove-hook 'window-scroll-functions #'lf-update--viewports)
    (setq display-buffer-alist (cdr display-buffer-alist))
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

(defun lf-refresh (&optional rebuild filter no-revert)
  "Reset lf. With optional prefix ARG (\\[universal-argument])
also rebuild lf layout."
  (interactive "P")
  (when rebuild
    (lf-build--parent-windows)
    (lf-build--preview-window)
    (lf-build--header-frame))
  (unless no-revert (revert-buffer))
  (when filter (lf-update--filter))
  (lf-update--padding)
  (lf-update--icons)
  (lf-update--line)
  (lf-update--preview)
  (lf-update--header)
  (lf-update--footer))

;;;; Core utilities

;;;###autoload
(defun lf (&optional path)
  "Launch dired in lf-mode."
  (interactive)
  (let* ((file (or path buffer-file-name))
         (dir (if file (expand-file-name (file-name-directory file))
                (expand-file-name default-directory))))
    (lf-init)
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
            (set-frame-parameter nil 'lf-index-path
                                 (or (dired-get-filename nil t) entry))
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
        (add-hook 'dired-after-readin-hook overrider)
      (remove-hook 'dired-after-readin-hook overrider))))

;;;###autoload
(define-minor-mode lf-minibuf-preview-mode
  "Show lf preview when minibuf."
  :group 'lf :global t
  (when lf-minibuf--get-candidate
    (if lf-minibuf-preview-mode
        (progn
          (add-hook 'minibuffer-setup-hook #'lf-minibuf-preview-create)
          (add-hook 'minibuffer-exit-hook #'lf-minibuf-preview-teardown)
          (advice-add 'vertico--exhibit :around #'lf-minibuf--update-advice)
          (advice-add 'selectrum--update :around #'lf-minibuf--update-advice))
      (remove-hook 'minibuffer-setup-hook #'lf-minibuf-preview-create)
      (remove-hook 'minibuffer-exit-hook #'lf-minibuf-preview-teardown)
      (advice-remove 'vertico--exhibit #'lf-minibuf--update-advice)
      (advice-remove 'selectrum--update #'lf-minibuf--update-advice))))

(define-derived-mode lf-mode dired-mode "Lf"
  "Major mode emulating the lf file manager in `dired'."
  :group 'lf
  :interactive nil
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq header-line-format (propertize " " 'display
                                       `(height ,(* 2 (1+ lf-line-padding))))))

(provide 'lf)

;;; lf.el ends here
