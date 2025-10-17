;;; init-elixir.el --- Nix mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :defer t
  :mode "\\.ex\\'"
  :mode "\\.exs\\'")

(add-to-list 'major-mode-remap-alist '(elixir-ts-mode . elixir-mode))
(provide 'init-elixir)
;;; init-elixir.el ends here.
