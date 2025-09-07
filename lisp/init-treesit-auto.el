;;; init-treesit-auto.el --- VERTical Interactive COmpletion  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit-auto
  :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))
(provide 'init-treesit-auto)
;;; init-treesit-auto.el ends here.
