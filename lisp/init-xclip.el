;;; init-xclip.el --- A Git Porcelain inside Emacs-*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package xclip
  :ensure t
  :defer t
  :hook
  (elpaca-after-init . xclip-mode))     ;; Enable xclip mode after initialization.
(provide 'init-xclip)
;;; init-xclip.el ends here.
