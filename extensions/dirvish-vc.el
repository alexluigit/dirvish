;;; dirvish-vc.el --- Version-control integration for Dirvish -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 2.3.0
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Show version-control info such as git commit message at root window and git
;; diff at preview window in Dirvish.

;;; Code:

(require 'dirvish)
(require 'transient)
(define-fringe-bitmap 'dirvish-vc-gutter [250] nil nil '(center repeated))

(defclass dirvish-vc-preview (transient-switches) ()
  "Class for dirvish vc-* preview dispatchers.")

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

(defvar dirvish-vc--always-ignored "/node_modules"
  "Always ignore folders matches this regex, as they may choke Emacs.")

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

(cl-defmethod dirvish-data-for-dir
  (dir buffer inhibit-setup
       &context ((dirvish-prop :vc-backend) symbol)
       &context ((dirvish-prop :remote) symbol))
  "Fetch data for DIR in BUFFER.
It is called when `:vc-backend' is included in DIRVISH-PROPs while
`:remote' is not, i.e. a local version-controlled directory.  Run
`dirvish-setup-hook' after data parsing unless INHIBIT-SETUP is non-nil."
  (dirvish--make-proc
   `(prin1
     (let* ((hs (make-hash-table))
            (bk ',(dirvish-prop :vc-backend))
            (info (vc-call-backend bk 'mode-line-string ,dir)))
       ;; keep this until `vc-git' fixed upstream.  See: #224 and #273
       (advice-add #'vc-git--git-status-to-vc-state :around
                   (lambda (fn codes) (apply fn (list (delete-dups codes)))))
       (dolist (file (directory-files ,dir t nil t))
         (let ((state (if (string-suffix-p ,dirvish-vc--always-ignored file)
                          'ignored (vc-state-refresh file bk)))
               (msg (and (eq bk 'Git)
                         (shell-command-to-string
                          (format "git log -1 --pretty=%%s %s"
                                  (shell-quote-argument file))))))
           (puthash (secure-hash 'md5 file)
                    `(:vc-state ,state :git-msg ,msg) hs)))
       (cons info hs)))
   (lambda (p _)
     (pcase-let ((`(,buf . ,inhibit-setup) (process-get p 'meta))
                 (`(,info . ,data) (with-current-buffer (process-buffer p)
                                     (read (buffer-string)))))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (maphash
            (lambda (k v)
              (let ((orig (gethash k dirvish--dir-data)))
                (setf (plist-get orig :vc-state) (plist-get v :vc-state))
                (setf (plist-get orig :git-msg) (plist-get v :git-msg))
                (puthash k orig dirvish--dir-data)))
            data)
           (dirvish-prop :vc-info info)
           (unless inhibit-setup (run-hooks 'dirvish-setup-hook))
           (dirvish--redisplay))))
     (delete-process p)
     (dirvish--kill-buffer (process-buffer p)))
   nil 'meta (cons buffer inhibit-setup)))

(cl-defmethod transient-infix-set ((obj dirvish-vc-preview) value)
  "Set relevant value in DIRVISH-VC-PREVIEW instance OBJ to VALUE."
  (oset obj value value)
  (let* ((dv (dirvish-curr))
         (buf (current-buffer))
         (old-layout (dv-curr-layout dv))
         (new-layout (unless old-layout (dv-ff-layout dv)))
         (new-dps (seq-difference
                   dirvish-preview-dispatchers '(vc-diff vc-log vc-blame))))
    (when value (push (intern (format "%s" value)) new-dps))
    (dirvish-prop :preview-dps (dirvish--preview-dps-validate new-dps))
    (if (not new-layout)
        (dirvish--preview-update dv (dirvish-prop :index))
      (quit-window nil (dv-root-window dv))
      (delete-window transient--window)
      (setf (dv-curr-layout dv) new-layout)
      (switch-to-buffer buf)
      (dirvish--build-layout dv))))

(transient-define-infix dirvish-vc-preview-ifx ()
  :description "Preview style"
  :class 'dirvish-vc-preview
  :argument-format "vc-%s"
  :argument-regexp "\\(vc-\\(log\\|diff\\|blame\\)\\)"
  :choices '("log" "diff" "blame"))

(dirvish-define-attribute vc-state
  "The version control state at left fringe.
This attribute only works on graphic displays."
  :when (and (symbolp (dirvish-prop :vc-backend)) (not (dirvish-prop :remote)))
  (let ((ov (make-overlay l-beg l-beg)))
    (when-let* ((state (dirvish-attribute-cache f-name :vc-state))
                (face (alist-get state dirvish-vc-state-face-alist))
                (display `(left-fringe dirvish-vc-gutter . ,(cons face nil))))
      (overlay-put ov 'before-string (propertize " " 'display display)))
    `(ov . ,ov)))

(dirvish-define-attribute git-msg
  "Display short git log."
  :when (and (eq (dirvish-prop :vc-backend) 'Git) (not (dirvish-prop :remote)))
  :setup (dirvish-prop :gm-chop
           (seq-reduce (lambda (acc i) (cl-incf acc (nth 2 i)))
                       (dirvish-prop :attrs) 0))
  (let* ((msg-raw (dirvish-attribute-cache f-name :git-msg))
         (msg (if (>= (length msg-raw) 1) (substring msg-raw 0 -1) ""))
         (face (or hl-face 'dirvish-git-commit-message-face))
         (chop (dirvish-prop :gm-chop)) (mlen (length msg)) (stop t)
         (limit (- (floor (* (if (< w-width 70) 0.48 0.6) w-width)) chop))
         (count 0) (whole (concat " " msg (make-string w-width ?\ ))) str len)
    (cond ((or (not msg-raw) (< w-width 30)) (setq str ""))
          ((and (>= w-width 30) (< w-width 50)) (setq str (propertize " …  ")))
          (t (setq str "" stop (<= limit 0))))
    (while (not stop) ; prevent multibyte string taking too much space
      (setq str (substring whole 0 count))
      (if (>= (- limit (string-width str)) 1)
          (cl-incf count)
        (setq str (concat str (if (> count mlen) "  " "… ")) stop t)))
    (add-face-text-property 0 (setq len (length str)) face t str)
    (add-text-properties 0 len `(help-echo ,msg) str)
    `(right . ,str)))

(dirvish-define-preview vc-diff (ext)
  "Use output of `vc-diff' as preview."
  (when (and (symbolp (dirvish-prop :vc-backend))
             (not (member ext dirvish-binary-exts))
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                       ((symbol-function 'message) #'ignore))
               (vc-diff)))
    '(buffer . "*vc-diff*")))

(dirvish-define-preview vc-log ()
  "Use output of `vc-print-log' as preview."
  (when (and (symbolp (dirvish-prop :vc-backend))
             (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
               (prog1 t (vc-print-log))))
    '(buffer . "*vc-change-log*")))

(dirvish-define-preview vc-blame (file ext preview-window dv)
  "Use output of `vc-annotate' (file) or `vc-dir' (dir) as preview."
  (when-let* ((bk (dirvish-prop :vc-backend))
              ((symbolp bk))
              (display-buffer-alist
               '(("\\*\\(Annotate \\|vc-dir\\).*\\*"
                  (display-buffer-same-window)))))
    (if (file-directory-p file)
        (with-selected-window preview-window
          (vc-dir file bk)
          (cl-pushnew vc-dir-process-buffer (dv-preview-buffers dv))
          `(buffer . ,(current-buffer)))
      (when-let* ((file (and (not (member ext dirvish-binary-exts))
                             (not (memq (vc-state file bk)
                                        '(unregistered ignored)))
                             file))
                  (f-buf (cdr (dirvish--find-file-temporarily file)))
                  ((bufferp f-buf)))
        (cl-pushnew f-buf (dv-preview-buffers dv))
        (with-selected-window preview-window
          (with-current-buffer f-buf
            (cl-letf (((symbol-function 'message) #'ignore))
              (vc-annotate file nil 'fullscale nil nil bk))
            (cl-pushnew (window-buffer) (dv-preview-buffers dv))
            `(buffer . ,(window-buffer))))))))

(dirvish-define-mode-line vc-info
  "Version control info such as git branch."
  (when-let* (((> (window-width) 30))
              (info-seq (dirvish-prop :vc-info))
              (info (copy-sequence info-seq)))
    (unless (dirvish--selected-p)
      (put-text-property 0 (length info) 'face 'dirvish-inactive info))
    info))

;;;###autoload (autoload 'dirvish-vc-menu "dirvish-vc" nil t)
(transient-define-prefix dirvish-vc-menu ()
  "Help menu for features in `dirvish-vc'."
  :init-value
  (lambda (o) (oset o value (mapcar (lambda (d) (format "%s" d))
                               dirvish-preview-dispatchers)))
  [:description
   (lambda () (dirvish--format-menu-heading "Version control commands"))
   ("v" dirvish-vc-preview-ifx
    :if (lambda () (symbolp (dirvish-prop :vc-backend))))
   ("n" "Do the next action" dired-vc-next-action
    :if (lambda () (symbolp (dirvish-prop :vc-backend))))
   ("c" "Create repo" vc-create-repo)])

(provide 'dirvish-vc)
;;; dirvish-vc.el ends here
