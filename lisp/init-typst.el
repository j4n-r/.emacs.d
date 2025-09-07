;;; init-typst.el --- Nix mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package typst-ts-mode
  :ensure t
  :elpaca (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

(provide 'init-typst)
;;; init-typst.el ends here.
