;;; init-ediff.el --- Ediff settings and Magit review -*- lexical-binding: t; -*-
;;; Commentary:
;; Review a commit: M-x my-ediff-review-commit
;; Review staged changes: M-x my-ediff-review-staged
;; In the Ediff control window, n/p cross file boundaries; q quits.
;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'magit-ediff)

(use-package ediff
  :ensure nil
  :demand t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(defvar my-ediff-review-pending nil
  "Dynamically bound state for a starting review session.")

(defvar-local my-ediff-review-state nil
  "Review state: file markers, current index, direction, and kind.")

(defun my-ediff-review--files (&optional staged-only)
  "Collect file markers, restricting to staged files if STAGED-ONLY."
  (let (files)
    (cl-labels
        ((collect (section staged)
           (let ((staged (or staged
                             (eq (oref section type) 'staged))))
             (if (eq (oref section type) 'file)
                 (when (or (not staged-only) staged)
                   (push (copy-marker (oref section start)) files))
               (dolist (child (oref section children))
                 (collect child staged))))))
      (collect magit-root-section nil))
    (vconcat (nreverse files))))

(defun my-ediff-review-commit ()
  "Review the displayed Magit commit with n/p across files."
  (interactive)
  (unless (derived-mode-p 'magit-revision-mode)
    (user-error "Open a commit with magit-show-commit first"))
  (let ((files (my-ediff-review--files)))
    (when (zerop (length files))
      (user-error "No file sections in this commit buffer"))
    (my-ediff-review--open files 0 1 'commit)))

(defun my-ediff-review-staged ()
  "Review staged changes with n/p across files."
  (interactive)
  (unless (derived-mode-p 'magit-status-mode)
    (user-error "Run this from a Magit status buffer"))
  (let ((files (my-ediff-review--files t)))
    (when (zerop (length files))
      (user-error "No staged files"))
    (my-ediff-review--open files 0 1 'staged)))

(defun my-ediff-review--open (files index direction kind)
  "Open FILES at INDEX, entering from DIRECTION for review KIND."
  (let ((marker (aref files index))
        (my-ediff-review-pending (list files index direction kind))
        (ediff-startup-hook
         (cons #'my-ediff-review--setup ediff-startup-hook)))
    (unless (marker-buffer marker)
      (user-error "The source Magit buffer was closed"))
    (pop-to-buffer (marker-buffer marker))
    (goto-char marker)
    (if (eq kind 'staged)
        (magit-ediff-show-staged (magit-current-file))
      (magit-ediff-dwim))))

(defun my-ediff-review--setup ()
  "Install navigation for this review's Ediff control buffer."
  (setq-local my-ediff-review-state my-ediff-review-pending)
  ;; Keep ordinary Ediff sessions' bindings unchanged.
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "n") #'my-ediff-review-next)
  (local-set-key (kbd "p") #'my-ediff-review-previous)
  (when (> ediff-number-of-differences 0)
    (ediff-jump-to-difference
     (if (> (nth 2 my-ediff-review-state) 0)
         1
       ediff-number-of-differences))))

(defun my-ediff-review--step (direction)
  "Move one difference in DIRECTION, crossing file boundaries."
  (unless my-ediff-review-state
    (user-error "Not in a custom Ediff review session"))
  (if (if (> direction 0)
          (< ediff-current-difference
             (1- ediff-number-of-differences))
        (> ediff-current-difference 0))
      (if (> direction 0)
          (ediff-next-difference)
        (ediff-previous-difference))
    (pcase-let* ((`(,files ,index ,_ ,kind) my-ediff-review-state)
                 (next (+ index direction)))
      (if (or (< next 0) (>= next (length files)))
          (message "Reached the %s of the review"
                   (if (> direction 0) "end" "beginning"))
        (unless (marker-buffer (aref files next))
          (user-error "The source Magit buffer was closed"))
        (ediff-really-quit nil)
        (my-ediff-review--open files next direction kind)))))

(defun my-ediff-review-next ()
  "Move to the next difference, crossing files when necessary."
  (interactive)
  (my-ediff-review--step 1))

(defun my-ediff-review-previous ()
  "Move to the previous difference, crossing files when necessary."
  (interactive)
  (my-ediff-review--step -1))

(with-eval-after-load 'magit-ediff
  (transient-append-suffix 'magit-ediff "c"
    '("a" "Review all"
      my-ediff-review
      :description
      (lambda ()
        (if (derived-mode-p 'magit-status-mode)
            "Review all staged"
          "Review whole commit")))))

(defun my-ediff-review ()
  "Review the commit or staged changes according to the current buffer."
  (interactive)
  (cond
   ((derived-mode-p 'magit-revision-mode)
    (my-ediff-review-commit))
   ((derived-mode-p 'magit-status-mode)
    (my-ediff-review-staged))
   (t
    (user-error "Open a Magit status or commit buffer first"))))

(provide 'init-ediff)
;;; init-ediff.el ends here.
