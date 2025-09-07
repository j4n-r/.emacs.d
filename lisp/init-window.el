;;; init-window.el --- configure windows internal package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
        ("\\magit.*"
        (display-buffer-in-side-window)
        (window-width . 1)
        (side . right)
        (slot . -1))


        ("\\COMMIT_EDITMSG"
        (display-buffer-in-side-window)
        (window-width . 1)
        (side . left)
        (slot . 0))
        
        ("\\*vterm.*\\*"
        (display-buffer-reuse-window display-buffer-in-direction)
        (direction . right)
        (window-width . 1)
        (side . right)
        (slot . 0))

        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
        (display-buffer-in-side-window)
        (window-height . 0.25)
        (side . bottom)
        (slot . 0))

        ;; Example configuration for the LSP help buffer,
        ;; keeps it always on bottom using 25% of the available space:
        ("\\*\\(lsp-help\\)\\*"
        (display-buffer-in-side-window)
        (window-height . 0.25)
        (side . bottom)
        (slot . 0))

        ;; Configuration for displaying various diagnostic buffers on
        ;; bottom 25%:
        ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
        (display-buffer-in-side-window)
        (window-height . 0.25)
        (side . bottom)
        (slot . 1))
     )))
(provide 'init-window)
;;; init-window.el ends here.
