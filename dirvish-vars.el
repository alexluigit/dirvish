;;; dirvish-vars.el --- Variables defined in dirvish -*- lexical-binding: t -*-

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

(define-obsolete-variable-alias 'dirvish-use-large-header 'dirvish-header-style "0.8")

(defcustom dirvish-header-style 'large
  "Display STYLE used for header in a full-frame dirvish instance.

STYLE should be one of these:
- nil, which means do not show the header.
- `large', show header in a window of 2 lines height.
- `normal', show header in a window of 1 line height."
  :group 'dirvish :type 'symbol
  :options '(nil large normal))

(defcustom dirvish-header-face-remap-alist
  '((default :background "#303030"))
  "Face remapping alist used in dirvish header window.
Beware that only full-frame dirvish uses header window. To
configure faces in dirvish parent windows, use
`dirvish-parent-face-remap-alist' instead.

See `face-remapping-alist' for more details."
  :group 'dirvish :type 'alist)

(defcustom dirvish-parent-face-remap-alist
  '((header-line :height 1.2 :box (:line-width 4 :color "#303030"))
    (highlight :inherit (highlight) :extend t))
  "Face remapping alist used in dirvish parent window.
This variable doesn't take effect in a dirvish header window. To
configure faces in dirvish header window, use
`dirvish-header-face-remap-alist' instead.

See `face-remapping-alist' for more details."
  :group 'dirvish :type 'alist)

(defcustom dirvish-header-text-fn 'dirvish-header-text
  "Function used to output a string that will show up as header."
  :group 'dirvish :type 'function)

(defvar dirvish-preview-setup-hook nil
  "Hook for preview buffer initialization.")

(defvar dirvish-activation-hook nil
  "Hook runs after activation of dirvish instance.")

;;;; Internal variables

(defconst dirvish-preview-delay 0.02)
(defconst dirvish-footer-repeat 0.1)
(defconst dirvish-header-wobbling-offset 2)
(defvar dirvish-history-ring (make-ring dirvish-history-length))
(defvar dirvish-preview-update-timer nil)
(defvar dirvish-repeat-timers '())
(defvar dirvish-IO-queue '())
(defvar-local dirvish-child-entry nil)
(defvar-local dirvish--curr-name nil)

(provide 'dirvish-vars)

;;; dirvish-vars.el ends here
