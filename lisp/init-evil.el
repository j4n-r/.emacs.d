;;; init-evil.el ---  extensible vi layer  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package evil
  :ensure t
  :defer t
  :preface
  (setq evil-respect-visual-line-mode t)
  :hook
  (elpaca-after-init . evil-mode)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-leader/in-all-states t)
  (evil-want-fine-undo t))

(use-package evil-collection
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))
(provide 'init-evil)
;;; init-evil.el ends here.
