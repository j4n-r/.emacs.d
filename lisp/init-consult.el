;;; init-consult.el --- provides search and navigation commands based on the Emacs completion function -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :ensure t
  :straight t
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(provide 'init-consult)
;;; init-consult.el ends here.
