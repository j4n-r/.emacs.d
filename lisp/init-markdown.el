;;; init-markdown.el --- markdown mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :defer t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)            ;; Use gfm-mode for README.md files.
  :init (setq markdown-command "multimarkdown")) ;; Set the Markdown processing command.
(provide 'init-markdown)
;;; init-markdown.el ends here.
