;;; init-markdown.el --- markdown mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)            ;; Use gfm-mode for README.md files.
  :hook (markdown-mode . markdown-live-preview-mode)
  :init
  (setq markdown-command "pandoc"))

(use-package grip-mode
  :ensure t
  :defer t
  :custom
  (grip-command 'go-grip)
  ;; Embedded WebKit requires a graphical frame with xwidget support.
  (grip-preview-in-webkit (display-graphic-p)))

(provide 'init-markdown)
;;; init-markdown.el ends here.
