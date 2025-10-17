;;; init-rust.el --- Rust mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :init
  (setq rust-mode-treesitter-derive t))


(provide 'init-rust)
;;; init-rust.el ends here.
