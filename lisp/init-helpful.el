;;; init-helpful.el --- Better help buffers -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package helpful
  :ensure t
  :bind
  (("C-h f"   . helpful-callable)   ;; functions + macros replacement
   ("C-h v"   . helpful-variable)   ;; variables
   ("C-h k"   . helpful-key)        ;; keys
   ("C-h x"   . helpful-command)    ;; commands
   ("C-c C-d" . helpful-at-point)   ;; symbol at point
   ("C-h F"   . helpful-function))) ;; functions only

(provide 'init-helpful)
;;; init-helpful.el ends here.
