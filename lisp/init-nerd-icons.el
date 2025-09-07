;;; init-orderless.el --- Nerd icon support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Provides Nerd Icons to be used with CORFU.
(use-package nerd-icons-corfu
  :ensure t
  :defer t
  :after (:all corfu))

;;; NERD ICONS Dired
;; The `nerd-icons-dired' package integrates nerd icons into the Dired mode,
;; providing visual icons for files and directories. This enhances the Dired
;; interface by making it easier to identify file types at a glance.
(use-package nerd-icons-dired
  :ensure t                               ;; Ensure the package is installed.
  :defer t                                ;; Load the package only when needed to improve startup time.
  :hook
  (dired-mode . nerd-icons-dired-mode))


;; NERD ICONS COMPLETION
;; The `nerd-icons-completion' package enhances the completion interfaces in
;; Emacs by integrating nerd icons with completion frameworks such as
;; `marginalia'. This provides visual cues for the completion candidates,
;; making it easier to distinguish between different types of items.
(use-package nerd-icons-completion
  :ensure t                               ;; Ensure the package is installed.
  :after (:all nerd-icons marginalia)     ;; Load after `nerd-icons' and `marginalia' to ensure proper integration.
  :hook
  (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))            ;; Activate nerd icons for completion interfaces.

(provide 'init-nerd-icons)
;;; init-nerd-icons.el ends here.
