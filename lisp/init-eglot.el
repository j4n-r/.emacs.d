;;; init-eglot.el --- built in LSP Server -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :ensure nil
  :hook
  (kotlin-ts-mode . eglot-ensure)
  (rust-ts-mode  . eglot-ensure)
  (python-ts-mode  . eglot-ensure)
  (tsx-ts-mode  . eglot-ensure)
  (typescript-ts-mode  . eglot-ensure)
  (js-ts-mode  . eglot-ensure)
  ;; (c-ts-mode  . eglot-ensure)
  (css-ts-mode  . eglot-ensure)
  (html-ts-mode  . eglot-ensure)
  (c++-ts-mode  . eglot-ensure)
  (typst-ts-mode  . eglot-ensure)
  (nix-mode  . eglot-ensure)

  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(typst-ts-mode . ("tinymist" "lsp"))
                 '(nix-mode . ("nil" "--stdio")))))

(provide 'init-eglot)
;;; init-eglot.el ends here.
