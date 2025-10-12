;;; init-rust.el --- Rust mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :init
  (setq rust-mode-treesitter-derive t)
  )


(add-to-list 'major-mode-remap-alist '(rust-ts-mode . rust-mode))

(provide 'init-rust)
;;; init-rust.el ends here.
