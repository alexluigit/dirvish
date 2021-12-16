;;; dirvish-vars.el --- Variables defined in dirvish. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; User custom options / internal variables defined in dirvish.

;;; Code:

;;;; User facing options

(require 'ring)

(defvar dirvish-preview-update-timer nil)

(defgroup dirvish nil
  "A better Dired."
  :group 'dired)

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
  `(("text/"                   (find-file-noselect . (t nil)))
    ("image/"                  ("convert" "-resize" "%s" "%i" "%T"))
    ("audio/"                  ("mediainfo" "%i"))
    ("video/"                  ("ffmpegthumbnailer" "-i" "%i" "-o" "%T" "-s 0"))
    (("iso" "bin" "exe" "gpg") ("*Preview Disable*"))
    (("zip")                   ("zipinfo" "%i"))
    (("zst" "tar")             ("tar" "-tvf" "%i"))
    (("epub")                  ("epub-thumbnailer" "%i" "%T" "1024"))
    (("pdf")                   ,(if (featurep 'pdf-tools)
                                    '(find-file-noselect '(t nil))
                                  '("pdftoppm" "-jpeg" "-f" "1" "-singlefile" "%i" "%t"))))
  "Determine how dirvish show preview for different MIME types."
  :group 'dirvish :type '(alist :value-type ((choice list string) list)))

(defcustom dirvish-cache-dir
  (concat (or (getenv "XDG_CACHE_HOME") (concat (getenv "HOME") "/.cache")) "/dirvish/")
  "Preview / thumbnail cache directory for dirvish."
  :group 'dirvish :type 'string)

(defcustom dirvish-history-length 30
  "Length of history dirvish will track."
  :group 'dirvish :type 'integer)

(defcustom dirvish-enable-preview t
  "When not-nil preview the selected file."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-depth 1
  "Number of directories up to traverse."
  :group 'dirvish :type 'integer)

(defcustom dirvish-width-parents 0.12
  "Fraction of frame width taken by parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-max-parent-width 0.25
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-body-padding 0.1
  "Line spacing for dirvish body."
  :group 'dirvish :type 'float)

(defcustom dirvish-footer-format "Sort: %S  Filter: %f  %d  %p%w%t %i"
  "Format for footer display."
  :group 'dirvish :type 'string)

(defcustom dirvish-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR).

Where DISK is path to a disk and TRASH-DIR is corresponding trash
directory."
  :group 'dirvish :type 'alist)

(defcustom dirvish-use-default-setup t
  "Whether use default config for dirvish buffer."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-show-icons t
  "When not-nil show file / dir icon."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-icons-monochrome t
  "Whether icon face use `face-at-point'."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-icons-v-offset 0.01
  "Icon vertical offset."
  :group 'dirvish :type 'float)

(defcustom dirvish-width-preview 0.65
  "Fraction of frame width taken by preview window."
  :group 'dirvish :type 'float)

(defcustom dirvish-preview-delay 0.02
  "Time in seconds to delay running preview file functions."
  :group 'dirvish :type 'float)

(defcustom dirvish-header-string-fn 'dirvish--header-string
  "Function used to output a string that will show up as header."
  :group 'dirvish :type 'function)

(defcustom dirvish-header-scale 1.25
  "Height of header line."
  :group 'dirvish :type 'number)

(defcustom dirvish-header-position
  (lambda (_)
    (let ((tab-h (tab-bar-height nil t))
          (fringe (or (frame-parameter nil 'internal-border-width) 0)))
      (cons 0 (+ tab-h fringe))))
  "A function determines dirvish header position.

Used as `:poshandler' for `posframe-show'."
  :group 'dirvish :type 'function)

;;;; Faces

(defgroup dirvish-faces nil
  "Faces used by Dirvish."
  :group 'dirvish :group 'faces)

(defface dirvish-body-face
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Face used for `dirvish-body-update'."
  :group 'dirvish-faces)

;;;; Internal variables

(defvar dirvish-header-width nil
  "Calculated header frame width.")

(defvar dirvish-header-buffer nil
  "Buffer for showing header line.")

(defvar dirvish--header-frame nil
  "Frame for showing header line.")

(defvar dirvish-preview-buffers ()
  "List with buffers of previewed files.")

(defvar dirvish-preview-setup-hook nil
  "Hooks for setting dirvish preview windows.")

(defvar dirvish-width-img 0
  "Calculated preview window width.  Used for image preview.")

(defvar dirvish-preview-window nil
  "Window contains file / directory preview.")

(defvar dirvish-preview-buffer nil)

(defvar dirvish-history-ring (make-ring dirvish-history-length)
  "History for `dirvish-find-file'.")

(defvar dirvish-initialized nil
  "Indicate if previous window config saved.")

(defvar dirvish-orig-recentf-list nil)

(defvar dirvish-footer-repeat 0.02
  "Time in seconds to repeat footer update.")

(defvar dirvish-sort-criteria ""
  "Default `ls' sorting switches.")

(defvar dirvish-window nil
  "Main dirvish window.  Adjacent to preview window.")

(defvar dirvish-parent-windows ()
  "List of parent windows.")

(defvar dirvish-parent-buffers ()
  "List with buffers of parent buffers.")

(defvar dirvish-frame-alist ()
  "List of frames using dirvish.")

(defvar dirvish-mode-hook nil
  "Hooks for setting dirvish parent windows.")

(defvar dirvish-repeat-timers '()
  "Timers with repeat flag need to be clean when exit.")

(defvar dirvish-IO-queue ())

(defvar-local dirvish-child-entry nil)

(defvar dirvish-index-path nil
  "Latest path in `dirvish-window'.")

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
    map)
  "Dirvish mode map.")

(defvar dirvish-advice-alist
  '((files         find-file                    dirvish-file-open--advice)
    (files         find-file-other-window       dirvish-file-open--advice)
    (dired         dired-find-file-other-window dirvish-other-window--advice)
    (dired         dired-readin                 dirvish-setup-dired-buffer--advice)
    (dired         dired-mark                   dirvish-update-line--advice)
    (dired         dired-flag-file-deletion     dirvish-update-line--advice)
    (dired         dired-goto-file              dirvish-update-line--advice)
    (dired         dired-internal-do-deletions  dirvish--deletion-advice)
    (dired         wdired-exit                  dirvish--refresh-advice)
    (dired         wdired-finish-edit           dirvish--refresh-advice)
    (dired         wdired-abort-changes         dirvish--refresh-advice)
    (dired-aux     dired-kill-line              dirvish--refresh-advice)
    (dired-aux     dired-do-kill-lines          dirvish--refresh-advice)
    (dired-aux     dired-create-directory       dirvish--refresh-advice)
    (dired-aux     dired-create-empty-file      dirvish--refresh-advice)
    (dired-aux     dired-do-create-files        dirvish--refresh-advice)
    (dired-aux     dired-insert-subdir          dirvish--refresh-advice)
    (dired-aux     dired-kill-subdir            dirvish--refresh-advice)
    (dired-aux     dired-rename-file            dirvish--revert-advice)
    (dired-narrow  dired--narrow-internal       dirvish--refresh-advice)
    (isearch       isearch-repeat-backward      dirvish--refresh-advice)
    (isearch       isearch-repeat-forward       dirvish--refresh-advice)
    (isearch       isearch-exit                 dirvish--refresh-advice)
    (find-dired    find-dired-sentinel          dirvish--refresh-advice)
    (evil          evil-refresh-cursor          dirvish-refresh-cursor--advice)
    (meow          meow--update-cursor          dirvish-refresh-cursor--advice)
    (autorevert    doom-auto-revert-buffer-h    ignore) ; For doom-emacs
    (lsp-mode      lsp-deferred                 ignore))
  "A list of file, adviced function, and advice function.

This variable is consumed by `dirvish--add-advices'.")

(provide 'dirvish-vars)

;;; dirvish-vars.el ends here
