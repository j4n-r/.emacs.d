;;; init-hl-todo.el --- highlight TODOs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package hl-todo
  :ensure t
  :defer t
  :hook
  (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
      '(("TODO"   . "#eb6f92")
        ("FIXME"  . "#eb6f92")
        ("BUG"    . "#eb6f92")
        ("NOTE"   . "#9ccfd8")
        ("DEBUG"  . "#f6c177"))))

(use-package consult-todo
  :ensure t
  :defer t)

(use-package magit-todos
  :ensure t
  :after magit
  :defer t
  :config (magit-todos-mode 1))

(provide 'init-hl-todo)

;;; init-hl-todo.el ends here.
