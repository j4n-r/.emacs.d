;;; init-pdf-tools.el --- PDF support  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (pdf-tools-install)
  :custom
  (pdf-view-resize-factor 1.15)
  )


(provide 'init-pdf-tools)
;;; init-pdf-tools.el ends here.
