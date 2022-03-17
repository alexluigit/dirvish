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

(defcustom dirvish-attributes '()
  "File attributes such as `file-size' showing in Dirvish file lines.
The attributes are defined by `dirvish-define-attribute', you can
get all available attributes by evaluating:

\(prog1 (mapc #'require `dirvish-extra-libs')
       (describe-variable 'dirvish--available-attrs))"
  :group 'dirvish :type '(repeat dirvish-attribute))

(define-obsolete-variable-alias 'dirvish-preview-cmd-alist 'dirvish-preview-dispatchers "0.9.7")

(defcustom dirvish-preview-dispatchers
  `(text gif image video audio epub archive ,(if (require 'pdf-tools nil t) 'pdf-tools 'pdf-preface))
  "List of preview dispatchers.
Preview dispatchers are defined by `dirvish-define-preview'.  It
holds a function that takes current filename and dirvish session
as arguments and gets called at runtime when the preview window
is available.  It controls how the preview content for certain
filetypes are generated, or it can decline to handle the file
name and leaving it for future dispatchers.  If none of the
dispatchers can handle the preview, the fallback dispatcher named
`default' is used.  For details see `dirvish-preview-dispatch'."
  :group 'dirvish :type 'hook)

(defcustom dirvish-preview-disabled-exts
  '("iso" "bin" "exe" "gpg" "elc" "eln")
  "Do not preview files end with these extensions."
  :group 'dirvish :type 'list)

(defcustom dirvish-cache-dir
  (concat (or (getenv "XDG_CACHE_HOME") (concat (getenv "HOME") "/.cache")) "/dirvish/")
  "Preview / thumbnail cache directory for dirvish."
  :group 'dirvish :type 'string)

(defcustom dirvish-async-listing-threshold most-positive-fixnum
  "Threshold to invoke async directory listing.
For example, if the value is 10000, then directories with over
10000 files will be opened asynchronously."
  :group 'dirvish :type 'integer)

(defvar dirvish--history-ring nil)
(defcustom dirvish-history-length 30
  "Length of directory visiting history Dirvish will track.
Set it to nil disables the history tracking."
  :group 'dirvish :type '(choice (integer nil))
  :set (lambda (k v) (set k v) (setq dirvish--history-ring (and v (make-ring v)))))

(defcustom dirvish-depth 1
  "Level of directories to traverse up."
  :group 'dirvish :type 'integer)

(defcustom dirvish-parent-max-width 0.12
  "The max width allocated to showing parent windows."
  :group 'dirvish :type 'float)

(defcustom dirvish-preview-width 0.6
  "Fraction of frame width taken by preview window."
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

(defcustom dirvish-header-string-function 'dirvish-default-header-string-fn
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

(defface dirvish-hl-line
  '((((class color) (background light)) :background "#8eecf4" :extend t)
    (((class color) (background dark)) :background "#004065" :extend t))
  "Face for Dirvish line highlighting."
  :group 'dirvish)

(defcustom dirvish-face-remap-alist
  '((header-line :height 1.04 :box (:line-width 4 :color "#303030")))
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
  :group 'dirvish :type '(choice (nil cons)))

(defvar dirvish-preview-setup-hook nil
  "Hook functions for preview buffer initialization.")

(defvar dirvish-activation-hook nil
  "Hook runs after activation of a Dirvish session.")

(defvar dirvish-deactivation-hook nil
  "Hook runs after deactivation of a Dirvish session.")

;;;; Internal variables

(defvar dirvish-advice-alist
  '((files         find-file                       dirvish-find-file-ad           :before)
    (dired         dired                           dirvish-dired-ad)
    (dired         dired-jump                      dirvish-dired-jump-ad)
    (dired         dired-find-file                 dirvish-find-file              :override)
    (dired         dired-find-alternate-file       dirvish-find-file              :override)
    (dired         dired-other-window              dirvish-dired-other-window-ad  :override)
    (dired         dired-other-tab                 dirvish-dired-other-tab-ad     :override)
    (dired         dired-other-frame               dirvish-dired-other-frame-ad   :override)
    (dired         dired-up-directory              dirvish-up-directory           :override)
    (dired         dired-sort-toggle-or-edit       dirvish-sort-by-criteria       :override)
    (dired         +dired/quit-all                 quit-window                    :override)
    (dired         dired-view-file                 dirvish--enlarge               :before)
    (dired         dired-internal-do-deletions     dirvish-deletion-ad)
    (dired-aux     dired-dwim-target-next          dirvish-dwim-target-next-ad    :override)
    (wdired        wdired-change-to-wdired-mode    dirvish-wdired-mode-ad         :after)
    (wdired        wdired-exit                     dirvish-setup                  :after)
    (wdired        wdired-finish-edit              dirvish-setup                  :after)
    (wdired        wdired-abort-changes            dirvish-setup                  :after)
    (find-dired    find-dired-sentinel             dirvish-find-dired-sentinel-ad :after)
    (recentf       recentf-track-opened-file       dirvish-ignore-ad)
    (recentf       recentf-track-closed-file       dirvish-ignore-ad)
    (winner        winner-save-old-configurations  dirvish-ignore-ad)
    (dired-subtree dired-subtree-remove            dirvish-subtree-remove-ad)
    (evil          evil-refresh-cursor             dirvish-refresh-cursor-ad)
    (meow          meow--update-cursor             dirvish-refresh-cursor-ad)
    (fd-dired      fd-dired                        dirvish-fd-dired-ad)
    (magit         magit-status-setup-buffer       dirvish--enlarge               :before)
    (lsp-mode      lsp-deferred                    dirvish-ignore-ad)))
(defvar dirvish-scopes '(:tab tab-bar--current-tab-index :frame selected-frame))
(defvar dirvish-hook-alist
  '((emacs window-selection-change-functions dirvish-reclaim)
    (emacs tab-bar-tab-pre-close-functions   dirvish--deactivate-for-tab)
    (emacs delete-frame-functions            dirvish--deactivate-for-frame)))
(defvar dirvish-debug-p nil)
(defvar dirvish-override-dired-mode nil)
(defvar dirvish-extra-libs '(dirvish-extras dirvish-vc))
(defconst dirvish--prefix-spaces 2)
(defconst dirvish--debouncing-delay 0.02)
(defconst dirvish--preview-img-threshold (* 1024 1024 0.5))
(defconst dirvish--preview-img-scale 0.92)
(defconst dirvish--repeat-interval 0.1)
(defconst dirvish--saved-new-tab-choice tab-bar-new-tab-choice)
(defconst dirvish--builtin-attrs '(hl-line symlink-target))
(defconst dirvish--header-remap-alist '((mode-line-inactive :inherit (mode-line) :height 1.99)))
(defconst dirvish--footer-remap-alist '((mode-line-inactive mode-line)))
(defconst dirvish--os-windows-p (memq system-type '(windows-nt ms-dos)))
(defvar dirvish--transient-dvs '())
(defvar dirvish--repeat-timers '())
(defvar dirvish--available-attrs '())
(defvar-local dirvish--dired-async-p nil)
(defvar-local dirvish--child-entry nil)
(defvar-local dirvish--curr-name nil)
(defvar-local dirvish--vc-backend nil)
(defvar-local dirvish--attrs-width `(,dirvish--prefix-spaces . 0))
(defvar-local dirvish--attrs-alist nil)
(put 'dired-subdir-alist 'permanent-local t)
(put 'dirvish--child-entry 'permanent-local t)
(put 'dirvish--curr-name 'permanent-local t)

(provide 'dirvish-options)
;;; dirvish-options.el ends here
