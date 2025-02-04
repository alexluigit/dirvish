;;; dirvish-vc.el --- Version-control integration for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.0.53
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Show version-control info such as git commit message at root window and git
;; diff at preview window in Dirvish.

;;; Code:

(require 'dirvish)
(define-fringe-bitmap 'dirvish-vc-gutter [250] nil nil '(center repeated))

(defclass dirvish-vc-preview (transient-switches) ()
  "Class for dirvish vc-* preview dispatchers.")

(defcustom dirvish-vc-state-fringe 3
  "The width of the fringe used to display the vc state indicator.
It is recommended to make this value greater than
`dirvish-window-fringe', which ensures that the `vc-state' attribute is
displayed properly."
  :group 'dirvish :type 'integer)

(defcustom dirvish-vc-state-face-alist
  '((up-to-date       . nil)
    (edited           . dirvish-vc-edited-state)
    (added            . dirvish-vc-added-state)
    (removed          . dirvish-vc-removed-state)
    (missing          . dirvish-vc-missing-state)
    (needs-merge      . dirvish-vc-needs-merge-face)
    (conflict         . dirvish-vc-conflict-state)
    (unlocked-changes . dirvish-vc-locked-state)
    (needs-update     . dirvish-vc-needs-update-state)
    (ignored          . nil)
    (unregistered     . dirvish-vc-unregistered-face))
  "Alist of (VC-STATE . FACE).
This value is consumed by `vc-state' attribute in Dirvish.  FACE is the
face used for that VC-STATE.  See `vc-state' in (in vc-hooks.el) for
detail explanation of these states."
  :group 'dirvish
  :type '(alist :key-type symbol :value-type (symbol :tag "Face")))

(defface dirvish-vc-needs-merge-face
  '((((background dark)) (:background "#500f29"))
    (t                   (:background "#efcbcf")))
  "Face used for `needs-merge' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-unregistered-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for `unregistered' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-git-commit-message-face
  '((t (:inherit dired-ignored :underline nil :background unspecified)))
  "Face for commit message overlays."
  :group 'dirvish)

(defface dirvish-vc-edited-state
  '((t :inherit vc-edited-state))
  "Face used for `edited' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-added-state
  '((t :inherit vc-locally-added-state))
  "Face used for `added' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-removed-state
  '((t :inherit vc-removed-state))
  "Face used for `removed' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-missing-state
  '((t :inherit vc-missing-state))
  "Face used for `missing' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-conflict-state
  '((t :inherit vc-conflict-state))
  "Face used for `conflict' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-locked-state
  '((t :inherit vc-locked-state))
  "Face used for `locked' vc state in the Dirvish buffer."
  :group 'dirvish)

(defface dirvish-vc-needs-update-state
  '((t :inherit vc-needs-update-state))
  "Face used for `needs-update' vc state in the Dirvish buffer."
  :group 'dirvish)

(defvar vc-dir-process-buffer)

(cl-defmethod transient-infix-set ((obj dirvish-vc-preview) value)
  "Set relevant value in DIRVISH-VC-PREVIEW instance OBJ to VALUE."
  (oset obj value value)
  (let* ((dv (dirvish-curr))
         (buf (current-buffer))
         (old-layout (car (dv-layout dv)))
         (new-layout (unless old-layout (cdr (dv-layout dv))))
         (new-dps (seq-difference
                   dirvish-preview-dispatchers '(vc-diff vc-log vc-blame))))
    (when value (push (intern (format "%s" value)) new-dps))
    (setq-local dirvish--working-preview-dispathchers
                (dirvish--preview-dps-validate new-dps))
    (if (not new-layout)
        (dirvish--preview-update dv (dirvish-prop :index))
      (quit-window nil (dv-root-window dv))
      (delete-window transient--window)
      (setcar (dv-layout dv) new-layout)
      (switch-to-buffer buf)
      (dirvish--init-session dv))))

(transient-define-infix dirvish-vc-preview-ifx ()
  :description "Preview style"
  :class 'dirvish-vc-preview
  :argument-format "vc-%s"
  :argument-regexp "\\(vc-\\(log\\|diff\\|blame\\)\\)"
  :choices '("log" "diff" "blame"))

(dirvish-define-attribute vc-state
  "The version control state at left fringe.
This attribute only works on graphic displays."
  ;; Avoid setting fringes constantly, which is expensive and slows down Emacs.
  (unless (= (car (window-fringes)) dirvish-vc-state-fringe)
    (set-window-fringes nil dirvish-vc-state-fringe dirvish-window-fringe))
  (let ((ov (make-overlay l-beg l-beg)))
    (when-let* (((dirvish-prop :vc-backend))
                (state (dirvish-attribute-cache f-name :vc-state))
                (face (alist-get state dirvish-vc-state-face-alist))
                (display `(left-fringe dirvish-vc-gutter . ,(cons face nil))))
      (overlay-put
       ov 'before-string (propertize " " 'display display)))
    `(ov . ,ov)))

(dirvish-define-attribute git-msg
  "Append git commit message to filename."
  :index 1
  :when (and (eq (dirvish-prop :vc-backend) 'Git)
             (not (dirvish-prop :remote))
             (> win-width 65))
  (let* ((info (dirvish-attribute-cache f-name :git-msg))
         (face (or hl-face 'dirvish-git-commit-message-face))
         (str (concat (substring (concat "  " info) 0 -1) " ")))
    (add-face-text-property 0 (length str) face t str)
    `(left . ,str)))

(dirvish-define-preview vc-diff (ext)
  "Use output of `vc-diff' as preview."
  (when (and (dirvish-prop :vc-backend)
             (not (member ext dirvish-media-exts))
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                       ((symbol-function 'message) #'ignore))
               (vc-diff)))
    '(buffer . "*vc-diff*")))

(dirvish-define-preview vc-log ()
  "Use output of `vc-print-log' as preview."
  (when (and (dirvish-prop :vc-backend)
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
               (prog1 t (vc-print-log))))
    '(buffer . "*vc-change-log*")))

(dirvish-define-preview vc-blame (file ext preview-window dv)
  "Use output of `vc-annotate' (file) or `vc-dir' (dir) as preview."
  (when-let* ((bk (dirvish-prop :vc-backend))
              (orig-buflist (buffer-list))
              (display-buffer-alist
               '(("\\*\\(Annotate \\|vc-dir\\).*\\*"
                  (display-buffer-same-window)))))
    (if (file-directory-p file)
        (with-selected-window preview-window
          (vc-dir file bk)
          (cl-pushnew vc-dir-process-buffer (dv-preview-buffers dv))
          `(buffer . ,(current-buffer)))
      (when-let* ((file (and (not (member ext dirvish-media-exts))
                             (not (memq (vc-state file bk)
                                        '(unregistered ignored)))
                             file))
                  (f-buf (cdr (dirvish--find-file-temporarily file)))
                  ((bufferp f-buf)))
        (unless (memq f-buf orig-buflist)
          (push f-buf (dv-preview-buffers dv)))
        (with-selected-window preview-window
          (with-current-buffer f-buf
            (cl-letf (((symbol-function 'message) #'ignore))
              (vc-annotate file nil 'fullscale nil nil bk))
            (cl-pushnew (window-buffer) (dv-preview-buffers dv))
            `(buffer . ,(window-buffer))))))))

(dirvish-define-mode-line vc-info
  "Version control info such as git branch."
  (when-let* (((> (window-width) 30))
              (bk (dirvish-prop :vc-backend))
              (ml-str (vc-call-backend bk 'mode-line-string default-directory))
              (bk-str (format "%s:" bk)))
    (format " %s %s "
            (propertize bk-str 'face 'bold)
            (propertize (substring ml-str (length bk-str))
                        'face 'font-lock-builtin-face))))

;;;###autoload (autoload 'dirvish-vc-menu "dirvish-vc" nil t)
(transient-define-prefix dirvish-vc-menu ()
  "Help menu for features in `dirvish-vc'."
  :init-value
  (lambda (o) (oset o value (mapcar (lambda (d) (format "%s" d))
                               dirvish-preview-dispatchers)))
  [:description
   (lambda () (dirvish--format-menu-heading "Version control commands"))
   ("v" dirvish-vc-preview-ifx
    :if (lambda () (dirvish-prop :vc-backend)))
   ("n" "Do the next action" dired-vc-next-action
    :if (lambda () (dirvish-prop :vc-backend)))
   ("c" "Create repo" vc-create-repo)])

(provide 'dirvish-vc)
;;; dirvish-vc.el ends here
