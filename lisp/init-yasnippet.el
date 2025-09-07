;;; init-yasnippet.el --- Snippets  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here.
