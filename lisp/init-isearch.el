;;; init-isearch.el --- configure isearch internal package -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package isearch
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
         ("C-S-s" . isearch-backward)))        ;; Bind C-S-s to backward isearch.
(provide 'init-isearch)
;;; init-isearch.el ends here.
