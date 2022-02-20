;;; dirvish-options.el --- Custom options and internal variables in dirvish -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file contains all user custom options, internal variables/constants and
;; dirvish mode keymap for Dirvish.

;;; Code:

;;;; User facing options

(require 'ring)
(require 'dired-x)

(defgroup dirvish nil
  "A better Dired."
  :group 'dired)

(defconst dirvish-icon-backend (or (require 'vscode-icon nil t) (require 'all-the-icons nil t)))
(defcustom dirvish-attributes `(,dirvish-icon-backend)
  "File attributes showing in Dirvish file lines."
  :group 'dirvish :type '(repeat dirvish-attribute))

(defcustom dirvish-enlarge-attributes '(git-msg)
  "File attributes that enlarge current window when present."
  :group 'dirvish :type '(repeat dirvish-attribute))

(define-obsolete-variable-alias 'dirvish-preview-cmd-alist 'dirvish-preview-dispatchers "0.9.7")

(defcustom dirvish-preview-dispatchers
  `(,(if (memq system-type '(windows-nt ms-dos)) 'directory-dired 'directory-exa)
    text gif image video audio epub archive
    ,(if (require 'pdf-tools nil t) 'pdf-tools 'pdf-preface))
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

(defcustom dirvish-depth 1
  "Level of directories to traverse up."
  :group 'dirvish :type 'integer)

(defcustom dirvish-parent-max-width 0.12
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-preview-width 0.65
  "Fraction of frame width taken by preview window."
  :group 'dirvish :type 'float)

(defcustom dirvish-side-display-alist
  '((side . left) (slot . -1) (window-width . 0.2))
  "Alist used in `display-buffer-in-side-window'."
  :group 'dirvish :type 'alist)

(define-obsolete-variable-alias 'dirvish-body-fontsize-increment 'dirvish-body-zoom "0.9.9")

(defcustom dirvish-body-zoom 0.05
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

(defcustom dirvish-header-string-function 'dirvish-default-header-string
  "Function that returns content (a string) in Dirvish header."
  :group 'dirvish :type 'function)

(define-obsolete-variable-alias 'dirvish-use-large-header 'dirvish-header-style "0.8")

(defcustom dirvish-header-style 'large
  "Display STYLE used for header in a full-frame dirvish instance.

STYLE should be one of these:
- nil, which means do not show the header.
- `normal', header has the same fontsize as body.
- `large', scale fontsize in header with 125%."
  :group 'dirvish :type 'symbol
  :options '(nil large normal))

(defcustom dirvish-face-remap-alist
  `((header-line :height ,(1+ dirvish-body-zoom) :box (:line-width 4 :color "#303030"))
    (highlight :inherit (highlight) :extend t)) ; line highlighting
  "Face remapping alist used in dirvish window.
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
format as `mode-line-format'.  Set it to nil hides the footer."
  :group 'dirvish :type '(choice nil cons))

(defvar dirvish-preview-setup-hook nil
  "Hook functions for preview buffer initialization.")

(defvar dirvish-activation-hook nil
  "Hook runs after activation of a Dirvish session.")

(defvar dirvish-deactivation-hook nil
  "Hook runs after deactivation of a Dirvish session.")

;;;; Internal variables

(defconst dirvish-header-line-height 1.99)
(defconst dirvish-debouncing-delay 0.02)
(defconst dirvish-preview-image-threshold (* 1024 1024 0.5))
(defconst dirvish-footer-repeat 0.1)
(defconst dirvish-saved-new-tab-choice tab-bar-new-tab-choice)
(defconst dirvish-temp-buffer (generate-new-buffer " *Dirvish temp*"))
(defvar dirvish-history-ring (make-ring dirvish-history-length))
(defvar dirvish-preview-update-timer nil)
(defvar dirvish-mode-line-update-timer nil)
(defvar dirvish-debug-p nil)
(defvar dirvish-override-dired-mode nil)
(defvar dirvish-repeat-timers '())
(defvar-local dirvish--child-entry nil)
(defvar-local dirvish--curr-name nil)
(defvar-local dirvish--vc-backend nil)
(put 'dired-subdir-alist 'permanent-local t)
(put 'dirvish--child-entry 'permanent-local t)
(put 'dirvish--curr-name 'permanent-local t)

(provide 'dirvish-options)
;;; dirvish-options.el ends here
