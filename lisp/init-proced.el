;;; init-proced.el --- configure internal proced package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package proced
  :ensure nil
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-sort 'pmem)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid  pcpu pmem rss start time state (args comm))))

(provide 'init-proced)
;;; init-proced.el ends here.
