;;; init-embark-consult.el --- Consult integration for Embark -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.
(provide 'init-embark-consult)
;;; init-embark-consult.el ends here.
