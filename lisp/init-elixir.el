;;; init-elixir.el --- Nix mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elixir-mode
  :ensure t
  :mode "\\.exs\\'"
  :mode "\\.ex\\'"
  :defer t)
(setq elixir-ts-mode-hook elixir-mode-hook)
(provide 'init-elixir)
;;; init-elixir.el ends here.
