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

  ;; TTY child frames do not draw Corfu's pixel border, so use a
  ;; contrasting popup background to keep them distinct from the buffer.
  ;; Keep this after `load-theme' so the theme cannot overwrite it.
  (with-eval-after-load 'corfu
    (set-face-attribute 'corfu-default nil
                        :background "#26233a" :foreground "#e0def4"
                        :extend t)
    (set-face-attribute 'corfu-current nil
                        :background "#403d52" :foreground "#e0def4")
    (set-face-attribute 'corfu-border nil :background "#6e6a86"))

  ;;(load-theme 'complinet)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package ample-theme
  :ensure t
  :defer t)


(provide 'init-themes)
;;; init-themes.el ends here.
