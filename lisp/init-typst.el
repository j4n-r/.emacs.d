;;; init-typst.el --- Typst mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode")
  :hook
  (typst-ts-mode . display-line-numbers-mode)
  :custom
  (typst-ts-mode-watch-options "--open"))

(provide 'init-typst)
;;; init-typst.el ends here.
