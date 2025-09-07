;;; init-themes.el --- Themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled


   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
   (load-theme 'doom-rose-pine t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'init-themes)
;;; init-themes.el ends here.
