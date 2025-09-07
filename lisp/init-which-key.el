;;; init-which-key.el --- configure which-key internal package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; `which-key' is an Emacs package that displays available keybindings in a
;; popup window whenever you partially type a key sequence. This is particularly
;; useful for discovering commands and shortcuts, making it easier to learn
;; Emacs and improve your workflow. It helps users remember key combinations
;; and reduces the cognitive load of memorizing every command.
(use-package which-key
  :ensure nil     ;; This is built-in, no need to fetch it.
  :defer t        ;; Defer loading Which-Key until after init.
  :hook
  (elpaca-after-init . which-key-mode)) ;; Enable which-key mode after initialization.
(provide 'init-which-key)
;;; init-which-key.el ends here.
