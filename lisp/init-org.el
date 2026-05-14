;;; init-org.el --- org mode options  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil ; do not try to install it as it is built-in
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-directory "~/notes")
  (setq org-agenda-files (list org-directory))

  ;; Learn about the ! and more by reading the relevant section of the
  ;; Org manual.  Evaluate: (info "(org) Tracking TODO state changes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))


(provide 'init-org)
;;; init-org.el ends here.
