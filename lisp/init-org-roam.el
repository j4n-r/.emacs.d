;;; init-org-roam.el --- configure isearch -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(server-start)
(provide 'init-org-roam)
;;; init-org-roam.el ends here.

