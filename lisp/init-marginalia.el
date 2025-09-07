;;; init-marginalia.el --- Provides Minibuffer annotations  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package marginalia
  :ensure t
  :hook
  (elpaca-after-init . marginalia-mode))

(provide 'init-marginalia)
;;; init-marginalia.el ends here.
