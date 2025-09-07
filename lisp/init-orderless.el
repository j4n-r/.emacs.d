;;; init-orderless.el --- orderless completion style  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'init-orderless)
;;; init-orderless.el ends here.
