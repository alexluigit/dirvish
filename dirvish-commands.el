;;; dirvish-commands.el --- dirvish core commands. -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Dirvish core commands.

;;; Code:

(require 'ring)

(require 'dirvish-vars)
(require 'dirvish-helpers)
(require 'dirvish-header)
(require 'dirvish-footer)
(require 'dirvish-init)

(defvar dirvish-preview-update-timer)

(defun dirvish-other-buffer ()
  "Replacement for `mode-line-other-buffer' in dirvish-mode."
  (interactive)
  (let ((one-window (frame-parameter nil 'dirvish-one-window)))
    (if one-window
        (switch-to-buffer (other-buffer) nil t)
      (dirvish-find-file (ring-ref dirvish-history-ring 1)))))

(defun dirvish-jump (file)
  "Replacement for `dired-jump'"
  (interactive (list (read-file-name "Jump to: "))) (dirvish-find-file-dwim file))

(defun dirvish-up-directory ()
  "Move to parent directory."
  (interactive)
  (let* ((current (expand-file-name default-directory))
         (parent (dirvish-get--parent current)))
    (if (string= parent current)
        (when (eq system-type 'windows-nt)
          (let* ((output (shell-command-to-string "wmic logicaldisk get name"))
                 (drives (cdr (split-string output)))
                 (drive (completing-read "Select drive: " drives)))
            (when drive (dirvish-find-file drive))))
      (dirvish-find-file parent t))))

(defun dirvish-go-top ()
  "Move to top of file list"
  (interactive)
  (goto-char (point-min)) (dired-next-line 1)
  (dirvish-next-file -1))

(defun dirvish-go-bottom ()
  "Move to bottom of file list"
  (interactive)
  (goto-char (point-max)) (dirvish-next-file 1))

(defun dirvish-next-file (arg)
  "Move lines in dirvish and initiate updates to preview window."
  (interactive "^p")
  (dired-next-line arg)
  (cond
   ((eobp) (unless (region-active-p) (forward-line -1)))
   ((bobp) (dired-next-line 1)))
  (when (dired-move-to-filename nil)
    (set-frame-parameter nil 'dirvish-index-path (dired-get-filename nil t))
    (dirvish-header-update)
    (dirvish-footer-update)
    (dirvish-debounce dirvish-preview-update dirvish-preview-delay)))

(defun dirvish-prev-file (arg)
  (interactive "^p")
  (dirvish-next-file (- 0 arg)))

(defun dirvish-show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list (completing-read "Select from history: "
                          (cl-remove-duplicates (ring-elements dirvish-history-ring)
                                                :test (lambda (x y) (or (null y) (equal x y)))))))
  (when history (dirvish-find-file history)))

(defun dirvish-new-frame (&optional path)
  "Make a new frame and launch dirvish."
  (interactive (list (read-file-name "Open in new frame: ")))
  (when (with-selected-window (selected-window) (eq major-mode 'dirvish-mode))
    (dirvish-quit))
  (let* ((after-make-frame-functions (lambda (f) (select-frame f)))
         (frame (make-frame '((name . "dirvish-emacs") (alpha . (100 50))))))
    (with-selected-frame frame (dirvish path))))

(defun dirvish-paste (&optional mode)
  "doc"
  (interactive)
  (let* ((regexp (dired-marker-regexp))
         (yanked-files ())
         (mode (or mode 'copy))
         case-fold-search)
    (cl-dolist (buf (seq-filter 'buffer-live-p dirvish-parent-buffers))
      (with-current-buffer buf
        (when (save-excursion (goto-char (point-min))
                              (re-search-forward regexp nil t))
          (setq yanked-files
                (append yanked-files (dired-map-over-marks (dired-get-filename) nil))))))
    (unless yanked-files (error "No files marked for paste."))
    (dirvish-internal-paste yanked-files mode)))

(defun dirvish-yank (&optional arg)
  (interactive "P")
  (if arg (dirvish-paste 'move) (dirvish-paste)))

(defun dirvish-change-level (&optional arg)
  (interactive "p")
  (setq dirvish-depth (or arg 1)) (dirvish-refresh t))

(defun dirvish-toggle-dotfiles ()
  "Show/hide dot-files."
  (interactive)
  (setq dirvish-show-hidden
        (cl-case dirvish-show-hidden
          ('all 'dot) ('dot 'dirvish) ('dirvish 'all)))
  (dirvish-refresh nil t))

(defun dirvish-toggle-preview ()
  "Show/hide preview window."
  (interactive)
  (setq dirvish-enable-preview (not dirvish-enable-preview))
  (dirvish-refresh t)
  (when dirvish-enable-preview
    (dired-hide-details-mode t)))

(defun dirvish-sort-by-criteria (criteria)
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
      (setq dirvish-sort-criteria (car sort-flag))
      (dired-sort-other switch)
      (dirvish-refresh))))

;;;###autoload
(defun dirvish-find-file-dwim (&rest args)
  "Call `dirvish-find-file' or `dired-find-file'."
  (if (derived-mode-p 'dirvish-mode)
      (apply 'dirvish-find-file args)
    (apply 'find-alternate-file args)))

;;;###autoload
(define-minor-mode dirvish-override-dired-jump
  "Override `dired-jump' with `dirvish-jump'."
  :group 'dirvish :global t
  (if dirvish-override-dired-jump
      (advice-add 'dired-jump :around #'dirvish-override-dired)
    (advice-remove 'dired-jump #'dirvish-override-dired)))

(provide 'dirvish-commands)

;;; dirvish-commands.el ends here
