;;; init-denote.el --- configure isearch -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package denote
  :ensure t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-save-buffers t)
  (setq denote-known-keywords '(
                                "uni"
                                "nix" "rust"
                                "book" "article"
                                "psychology" "philosophy"
                                "personal"
                                "temp" "metanote" "export"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(provide 'init-denote)
;;; init-denote.el ends here.

