;;; dirvish-options.el --- Custom options and internal variables in dirvish -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; This file contains all user custom options, internal variables/constants and
;;; dirvish mode keymap for Dirvish.

;;; Code:

;;;; User facing options

(require 'ring)
(require 'dired-x)

(defgroup dirvish nil
  "A better Dired."
  :group 'dired)

(define-obsolete-variable-alias 'dirvish-preview-cmd-alist 'dirvish-preview-dispatchers "0.9.7")

(defcustom dirvish-preview-dispatchers
  '(dirvish-preview-disable-dispatcher
    dirvish-preview-directory-exa-dispatcher
    dirvish-preview-text-dispatcher
    dirvish-preview-gif-dispatcher
    dirvish-preview-image-dispatcher
    dirvish-preview-video-dispatcher
    dirvish-preview-audio-dispatcher
    dirvish-preview-epub-dispatcher
    dirvish-preview-pdf-preface-dispatcher
    dirvish-preview-archive-dispatcher
    dirvish-preview-default-dispatcher)
  "List of preview dispatchers.
Preview dispatchers are used to determine how dirvish show
preview for different MIME or file extensions.  A preview
dispatcher is a function that takes current filename and dirvish
instance as arguments, it gets called at runtime when preview
window is available.  It can decide what elisp function or shell
command to use when generating the content in preview buffer for
certain filetypes, or it can decline to handle the filename
leaving it for future dispatchers.  For details see
`dirvish-preview-dispatch'."
  :group 'dirvish :type 'hook)

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

(defcustom dirvish-trash-dir-alist nil
  "An alist of (DISK . TRASH-DIR).

Where DISK is path to a disk and TRASH-DIR is its corresponding
trash directory.
For example, if you set it to:

'((\"/mnt/HDD/\" . \".Trash/files\")
  (\"/mnt/CloudDrive\" . \".recycle\"))

Dirvish take /mnt/HDD/.Trash/files as your trash can when you are
in /mnt/HDD directory or its child entries. This can speed up
file deletion when you have multiple disk drives."
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

(defcustom dirvish-header-line-format
  '((:eval (dirvish--header-line-path)))
  "Template for displaying header line in Dirvish instance.

The variable has the same format as `mode-line-format'."
  :group 'dirvish :type 'list)

(define-obsolete-variable-alias 'dirvish-use-large-header 'dirvish-header-style "0.8")

(defcustom dirvish-header-style 'large
  "Display STYLE used for header in a full-frame dirvish instance.

STYLE should be one of these:
- nil, which means do not show the header.
- `normal', header has the same fontsize as body.
- `large', scale fontsize in header with 125%."
  :group 'dirvish :type 'symbol
  :options '(nil large normal))

(defcustom dirvish-header-face-remap-alist
  '((mode-line-inactive :inherit (mode-line-active) :height 1.8))
  "Face remapping alist used in dirvish header window.
Beware that only full-frame dirvish uses header window.  To
configure faces in dirvish parent windows, use
`dirvish-parent-face-remap-alist' instead.

See `face-remapping-alist' for more details."
  :group 'dirvish :type 'alist)

(defcustom dirvish-parent-face-remap-alist
  '((header-line :height 1.2 :box (:line-width 4 :color "#303030"))
    (highlight :inherit (highlight) :extend t))
  "Face remapping alist used in dirvish parent window.
This variable doesn't take effect in a dirvish header window.  To
configure faces in dirvish header window, use
`dirvish-header-face-remap-alist' instead.

See `face-remapping-alist' for more details."
  :group 'dirvish :type 'alist)

(define-obsolete-variable-alias 'dirvish-footer-format 'dirvish-mode-line-format "0.9.9")

(defcustom dirvish-mode-line-format
  '(((:eval (dirvish--mode-line-sorter))
     (:eval (dirvish--mode-line-filter)))
    .
    ((:eval (dirvish--mode-line-index))))
  "Template for displaying mode line in Dirvish instance.

The value is a (LEFT . RIGHT) cons where LEFT/RIGHT has the same
format as `mode-line-format'.  Set it to nil disables Dirvish
footer."
  :group 'dirvish :type '(choice nil cons))

(defvar dirvish-preview-setup-hook nil
  "Hook for preview buffer initialization.")

(defvar dirvish-activation-hook nil
  "Hook runs after activation of dirvish instance.")

;;;; Internal variables

(defconst dirvish-debouncing-delay 0.02)
(defconst dirvish-preview-image-threshold (* 1024 1024 0.5))
(defconst dirvish-footer-repeat 0.1)
(defconst dirvish-saved-new-tab-choice tab-bar-new-tab-choice)
(defvar dirvish-history-ring (make-ring dirvish-history-length))
(defvar dirvish-preview-update-timer nil)
(defvar dirvish-mode-line-update-timer nil)
(defvar dirvish-repeat-timers '())
(defvar-local dirvish-child-entry nil)
(defvar-local dirvish--curr-name nil)
(put 'dired-subdir-alist 'permanent-local t)
(put 'dirvish-child-entry 'permanent-local t)

;;;; Mode keymap

(defvar dirvish-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap dired-do-redisplay]           'dirvish-change-level)
    (define-key map [remap dired-hide-details-mode]      'dirvish-toggle-preview)
    (define-key map [remap dired-find-file]              'dirvish-find-file)
    (define-key map [remap dired-find-alternate-file]    'dirvish-find-file)
    (define-key map [remap right-char]                   'dirvish-find-file)
    (define-key map [remap dired-up-directory]           'dirvish-up-directory)
    (define-key map [remap left-char]                    'dirvish-up-directory)
    (define-key map [remap end-of-buffer]                'dirvish-go-bottom)
    (define-key map [remap beginning-of-buffer]          'dirvish-go-top)
    (define-key map [remap dired-sort-toggle-or-edit]    'dirvish-sort-by-criteria)
    (define-key map [remap dired-view-file]              'dirvish-toggle-preview)
    (define-key map [remap quit-window]                  'dirvish-quit)
    (define-key map [remap +dired/quit-all]              'dirvish-quit) ; For doom-emacs
    map)
  "Dirvish mode map.")

(provide 'dirvish-options)
;;; dirvish-options.el ends here
