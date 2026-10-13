;;; dirvish-subtree-tests.el --- Subtree navigation regressions -*- lexical-binding: t -*-

;; Run with dependencies on load-path:
;; emacs -Q --batch -L . -L extensions -l tests/dirvish-subtree-tests.el \
;;   -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'dirvish-subtree)

(defmacro dirvish-subtree-test--with-directory (&rest body)
  "Run BODY in a temporary Dired buffer with root and nested files."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "dirvish-test-" t)))
          buffer)
     (unwind-protect
         (progn
           (make-directory (expand-file-name "sub" dir))
           (dolist (name '("a" "b" "c" "z" "sub/a" "sub/b" "sub/c"))
             (write-region "" nil (expand-file-name name dir) nil 'silent))
           (setq buffer (dired-noselect dir "-al"))
           (with-current-buffer buffer
             ;; Attribute collection is asynchronous UI work, not navigation.
             (cl-letf (((symbol-function 'dirvish--dir-data-async) #'ignore))
               ,@body)))
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (delete-directory dir t))))

(ert-deftest dirvish-subtree-revert-deleted-file-keeps-dired-position ()
  (dirvish-subtree-test--with-directory
    (let ((target (expand-file-name "b" dir)))
      (should (dired-goto-file target))
      (dirvish-prop :old-index target)
      (delete-file target)
      (dired-revert)
      (let ((position (point)))
        (run-hooks 'dirvish-after-revert-hook)
        (should (= (point) position))
        (should (< (point) (point-max)))))))

(ert-deftest dirvish-subtree-expand-hidden-file-keeps-position ()
  (dirvish-subtree-test--with-directory
    (let ((target (expand-file-name "b" dir)))
      ;; The file still exists, but is absent from the listing.
      (dired-remove-entry target)
      (should (file-exists-p target))
      (should (dired-goto-file (expand-file-name "c" dir)))
      (let ((position (point)))
        (should-not (dirvish-subtree-expand-to target))
        (should (= (point) position))))))

(ert-deftest dirvish-subtree-expand-existing-file-moves-point ()
  (dirvish-subtree-test--with-directory
    (let ((target (expand-file-name "b" dir)))
      (should (dired-goto-file (expand-file-name "a" dir)))
      (should (equal (dirvish-subtree-expand-to target) target))
      (should (equal (dired-get-filename) target)))))

(ert-deftest dirvish-subtree-expand-missing-child-keeps-position-after-insertion ()
  (dirvish-subtree-test--with-directory
    (should (dired-goto-file (expand-file-name "z" dir)))
    (let ((position (point-marker)))
      (unwind-protect
          (progn
            (should-not (dirvish-subtree-expand-to
                         (expand-file-name "sub/missing" dir)))
            (should dirvish-subtree--overlays)
            (should (= (point) (marker-position position)))
            (should (equal (dired-get-filename) (expand-file-name "z" dir))))
        (set-marker position nil)))))

(ert-deftest dirvish-subtree-expand-existing-child-moves-point ()
  (dirvish-subtree-test--with-directory
    (let ((target (expand-file-name "sub/b" dir)))
      (should (dired-goto-file (expand-file-name "c" dir)))
      (should (equal (dirvish-subtree-expand-to target) target))
      (should (equal (dired-get-filename) target)))))

(ert-deftest dirvish-subtree-revert-deleted-child-keeps-dired-position ()
  (dirvish-subtree-test--with-directory
    (let ((target (expand-file-name "sub/b" dir)))
      (should (dirvish-subtree-expand-to target))
      (dirvish-prop :old-index target)
      (delete-file target)
      (dired-revert)
      (let ((position (point-marker)))
        (unwind-protect
            (progn
              (run-hooks 'dirvish-after-revert-hook)
              (should (= (point) (marker-position position))))
          (set-marker position nil))))))

(provide 'dirvish-subtree-tests)
;;; dirvish-subtree-tests.el ends here
