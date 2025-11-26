;;; init-magit.el --- A Git Porcelain inside Emacs-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :defer t
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons) ;; Turns on magit nerd-icons
  (setopt magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-clone-submodules 'recursive))

(use-package transient
  :ensure t
  :defer t)

(use-package with-editor
  :ensure t
  :defer t)

(use-package forge
  :ensure t
  :defer t
  :after magit)

(provide 'init-magit)
;;; init-magit.el ends here.
