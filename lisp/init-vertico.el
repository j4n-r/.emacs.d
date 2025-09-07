;;; init-vertico.el --- VERTical Interactive COmpletion  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :ensure t
  :hook
  (elpaca-after-init . vertico-mode)           ;; Enable vertico after Emacs has initialized.
  :custom
  (vertico-count 15)                    ;; Number of candidates to display in the completion list.
  (vertico-resize nil)                  ;; Disable resizing of the vertico minibuffer.
  (vertico-cycle nil))                   ;; Do not cycle through candidates when reaching the end of the list.
;; :config
;; ;; Customize the display of the current candidate in the completion list.
;; ;; This will prefix the current candidate with “» ” to make it stand out.
;; ;; Reference: https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
;; (advice-add #'vertico--format-candidate :around
;;             (lambda (orig cand prefix suffix index _start)
;;               (setq cand (funcall orig cand prefix suffix index _start))
;;               (concat
;;                (if (= vertico--index index)
;;                    (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
;;                  "  ")
;;                cand))))
(provide 'init-vertico)
;;; init-vertico.el ends here.
