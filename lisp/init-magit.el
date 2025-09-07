;;; init-magit.el --- A Git Porcelain inside Emacs-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons) ;; Turns on magit nerd-icons
  :defer t)
(use-package transient
  :ensure t)
(use-package with-editor
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(provide 'init-magit)
;;; init-magit.el ends here.
