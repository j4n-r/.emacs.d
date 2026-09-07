;;; init-magit.el --- A Git Porcelain inside Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
         ("x" . magit-discard)
         ("k" . nil))
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)

  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function
        #'magit-restore-window-configuration)

  (setopt magit-clone-submodules 'recursive))

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
