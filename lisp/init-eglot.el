;;; init-eglot.el --- built in LSP Server -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package eglot
  :ensure nil
  :hook
  (kotlin-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (python-ts-mode  . eglot-ensure)
  (tsx-ts-mode  . eglot-ensure)
  (typescript-ts-mode  . eglot-ensure)
  (js-ts-mode  . eglot-ensure)
  ;; (c-ts-mode  . eglot-ensure)
  (css-ts-mode  . eglot-ensure)
  (html-ts-mode  . eglot-ensure)
  ;; (c++-ts-mode  . eglot-ensure)
  (typst-ts-mode  . eglot-ensure)
  (nix-mode  . eglot-ensure)
  (zig-mode . eglot-ensure)
  (elixir-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  :config
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist" "lsp")))
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("elixir-ls")))
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil" "--stdio"))))

(setq-default eglot-workspace-configuration
            '(:typescript (:format (:enable :json-false))))

(provide 'init-eglot)
;;; init-eglot.el ends here.
