;;; init-pulsar.el --- temporarily highlights the current line after a given function is invoked. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pulsar
  :defer t
  :ensure t
  :hook
  (elpaca-after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)
  (setq pulsar-region-face 'evil-ex-lazy-highlight)
  (setq pulsar-highlight-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'evil-window-right)
  (add-to-list 'pulsar-pulse-functions 'evil-window-left)
  (add-to-list 'pulsar-pulse-functions 'evil-window-top)
  (add-to-list 'pulsar-pulse-functions 'evil-window-bottom)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk)

  (add-to-list 'pulsar-pulse-region-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-region-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-region-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-region-functions 'evil-delete-line))
(provide 'init-pulsar)
;;; init-pulsar.el ends here.
