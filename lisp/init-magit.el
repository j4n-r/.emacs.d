;;; init-magit.el --- A Git Porcelain inside Emacs-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
         ("x" . magit-discard)
         ("k" . nil)) ;; Unbind 'k' so Meow can use it for movement
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function    #'magit-restore-window-configuration)
  (setopt magit-clone-submodules 'recursive)

  ;; Fix the Transient menu (swaps 'k' to 'x' perfectly in place)
  (transient-replace-suffix 'magit-file-dispatch "k"
    '("x" "Discard" magit-discard)))

(use-package transient
  :ensure t
  :defer t)

(use-package with-editor
  :ensure t
  :defer t)

(use-package forge
  :ensure t
  :after magit)

(provide 'init-magit)
;;; init-magit.el ends here.
