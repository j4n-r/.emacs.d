;;; init-eldoc-box.el --- displays ElDoc documentations in a childframe  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eldoc-box
  :ensure t
  :defer t
  :config
  ;; Internal child-frame borders are not visible in a TTY.  Give the body
  ;; its own surface color instead, while retaining a visible GUI border.
  (set-face-attribute 'eldoc-box-body nil
                      :background "#26233a" :foreground "#e0def4"
                      :extend t)
  (set-face-attribute 'eldoc-box-border nil :background "#6e6a86"))

(provide 'init-eldoc-box)
;;; init-eldoc-box.el ends here.
