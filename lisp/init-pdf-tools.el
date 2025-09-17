;;; init-pdf-tools.el --- PDF support  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (pdf-tools-install)
  (pdf-loader-activate))

(provide 'init-pdf-tools)
;;; init-pdf-tools.el ends here.
