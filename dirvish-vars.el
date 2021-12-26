;;; dirvish-vars.el --- Variables defined in dirvish. -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; User custom options / internal variables defined in dirvish.

;;; Code:

;;;; User facing options

(require 'ring)
(require 'dired-x)

(defgroup dirvish nil
  "A better Dired."
  :group 'dired)

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
  "Level of directories to traverse up."
  :group 'dirvish :type 'integer)

(defcustom dirvish-parent-max-width 0.12
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-preview-width 0.65
  "Fraction of frame width taken by preview window."
  :group 'dirvish :type 'float)

(defcustom dirvish-body-fontsize-increment 0.1
  "Font size increment in dirvish body.
For example, if this value is 0.1, the font size in dirvish body
will be scaled to 110% (1 + 0.1)."
  :group 'dirvish :type 'float)

(defcustom dirvish-footer-format "Sort: %S  Omit: %f  %d  %p%w%t %i"
  "Format for footer display."
  :group 'dirvish :type 'string)

(defcustom dirvish-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR).

Where DISK is path to a disk and TRASH-DIR is corresponding trash
directory."
  :group 'dirvish :type 'alist)

(defcustom dirvish-show-icons t
  "When not-nil show file / dir icon."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-icon-delimiter "\t"
  "A string attached to a dirvish icon."
  :group 'dirvish :type 'string)

(defcustom dirvish-icon-monochrome t
  "Whether icon face use `face-at-point'."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-icon-v-offset 0.01
  "Icon's vertical offset in dirvish body."
  :group 'dirvish :type 'float)

(defcustom dirvish-header-text-fn 'dirvish-header-text
  "Function used to output a string that will show up as header."
  :group 'dirvish :type 'function)

(defcustom dirvish-use-large-header t
  "Whether use a larger dirvish header (2 lines height) or not."
  :group 'dirvish :type 'boolean)

(defcustom dirvish-header-margin 0.1
  "The bottom margin of dirvish header.
The value of this number represent a proportion of header line
height in percentage.  This variable only takes effect
in a full frame dirvish instance."
  :group 'dirvish :type 'float)

(defvar dirvish-preview-setup-hook nil
  "Hook for preview buffer initialization.")

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

(defconst dirvish-preview-delay 0.02)
(defconst dirvish-footer-repeat 0.1)
(defvar dirvish-history-ring (make-ring dirvish-history-length))
(defvar dirvish-preview-update-timer nil)
(defvar dirvish-repeat-timers '())
(defvar dirvish-IO-queue '())
(defvar-local dirvish-child-entry nil)

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap dired-do-copy]                'dirvish-yank)
    (define-key map [remap dired-jump]                   'dirvish-jump)
    (define-key map [remap dired-do-redisplay]           'dirvish-change-level)
    (define-key map [remap dired-hide-details-mode]      'dirvish-toggle-preview)
    (define-key map [remap dired-find-file]              'dirvish-find-file)
    (define-key map [remap dired-find-alternate-file]    'dirvish-find-file)
    (define-key map [remap right-char]                   'dirvish-find-file)
    (define-key map [remap dired-up-directory]           'dirvish-up-directory)
    (define-key map [remap left-char]                    'dirvish-up-directory)
    (define-key map [remap next-line]                    'dirvish-next-file)
    (define-key map [remap dired-next-line]              'dirvish-next-file)
    (define-key map [remap previous-line]                'dirvish-prev-file)
    (define-key map [remap dired-previous-line]          'dirvish-prev-file)
    (define-key map [remap end-of-buffer]                'dirvish-go-bottom)
    (define-key map [remap beginning-of-buffer]          'dirvish-go-top)
    (define-key map [remap dired-sort-toggle-or-edit]    'dirvish-sort-by-criteria)
    (define-key map [remap revert-buffer]                'dirvish-reset)
    (define-key map [remap dired-view-file]              'dirvish-toggle-preview)
    (define-key map [remap quit-window]                  'dirvish-quit)
    (define-key map [remap +dired/quit-all]              'dirvish-quit) ; For doom-emacs
    map)
  "Dirvish mode map.")

(provide 'dirvish-vars)

;;; dirvish-vars.el ends here
