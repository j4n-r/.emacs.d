;;; init-diff-hl.el ---  highlights uncommitted changes on the side of the window -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)                          ;; Enable Diff-HL mode globally.
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  ;; :hook
  ;; (diff-hl-mode . (lambda ()
  ;;                   (when (and buffer-file-name (vc-backend buffer-file-name))
  ;;                     (diff-hl-flydiff-mode)     ;; Automatically refresh diffs.
  ;;                     (diff-hl-margin-mode))))   ;; Show diff indicators in the margin.
  ;; :custom
  ;; (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  ;; (diff-hl-margin-symbols-alist '((insert . "┃") ;; Customize symbols for each change type.
  ;;                                  (delete . "-")
  ;;                                  (change . "┃")
  ;;                                  (unknown . "┆")
  ;;                                  (ignored . "i"))))

(provide 'init-diff-hl)
;;; init-diff-hl.el ends here.

