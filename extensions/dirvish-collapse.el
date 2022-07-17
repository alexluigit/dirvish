;;; dirvish-collapse.el --- Collapse unique nested paths -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Alex Lu
;; Author : Alex Lu <https://github.com/alexluigit>
;; Version: 1.9.23
;; Keywords: files, convenience
;; Homepage: https://github.com/alexluigit/dirvish
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides `collapse' attribute to reveal unique nested paths.

;;; Code:

(require 'dirvish)

(defface dirvish-collapse-dir-face
  '((t (:inherit dired-directory)))
  "Face used for directories in `collapse' attribute."
  :group 'dirvish)

(defface dirvish-collapse-empty-dir-face
  '((t (:inherit shadow)))
  "Face used for empty directories in `collapse' attribute."
  :group 'dirvish)

(defface dirvish-collapse-file-face
  '((t (:inherit default)))
  "Face used for files in `collapse' attribute."
  :group 'dirvish)

(defun dirvish-collapse--cache (f-name)
  "Cache collapse state for file F-NAME."
  (dirvish-attribute-cache f-name :collapse
    (let ((path f-name) should-collapse files dirp)
      (while (and (setq dirp (file-directory-p path))
                  (setq files (ignore-errors (directory-files path t)))
                  (= 3 (length files)))
        (setq should-collapse t path (nth 2 files)))
      (cond
       ((and (eq (length files) 2) (not should-collapse)) (cons 'empty t))
       (should-collapse
        (let* ((path (substring path (1+ (length f-name))))
               (segs (split-string path "/"))
               (head (format "|%s|" (mapconcat #'concat (butlast segs) "|")))
               (tail (car (last segs)))
               (tail-face
                (if dirp 'dirvish-collapse-dir-face 'dirvish-collapse-file-face)))
          (and (equal head "||") (setq head "|"))
          (add-face-text-property 0 (length head) 'dirvish-collapse-dir-face nil head)
          (add-face-text-property 0 (length tail) tail-face nil tail)
          (cons head tail)))
       (t (cons nil nil))))))

(dirvish-define-attribute collapse
  "Collapse unique nested paths."
  (:if (and (eq (dv-root-window dv) (selected-window))
            (not (dirvish-prop :fd-dir))
            (or (not (dirvish-prop :tramp))
                (tramp-local-host-p (dirvish-prop :tramp)))))
  (when-let* ((cache (dirvish-collapse--cache f-name))
              (head (car cache))
              (tail (cdr cache)))
    (let* ((emptyp (eq head 'empty))
           (o (make-overlay (if emptyp f-beg f-end) f-end)))
      (if emptyp
          (overlay-put o 'face 'dirvish-collapse-empty-dir-face)
        (let* ((str (concat head tail))
               (len (length str))
               (remain (max (- remain f-wid) 0))
               (overflow (< remain len)))
          (and overflow (setq str (substring str 0 remain)))
          (and hl-face (add-face-text-property 0 (if overflow remain len) hl-face nil str))
          (overlay-put o 'after-string str)))
      o)))

(provide 'dirvish-collapse)
;;; dirvish-collapse.el ends here
