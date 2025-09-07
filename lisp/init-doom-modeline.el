;;; init-doom-modeline.el -- Pretty modeline --  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :ensure t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)  ;; Set the buffer file name style to just the buffer name (without path).
  (doom-modeline-project-detection 'project)           ;; Enable project detection for displaying the project name.
  (doom-modeline-buffer-name t)                        ;; Show the buffer name in the mode line.
  (doom-modeline-vcs-max-length 25)                    ;; Limit the version control system (VCS) branch name length to 25 characters.
  :config
  (setq doom-modeline-icon t)                      ;; Enable icons in the mode line if nerd fonts are used.
  (setq doom-modeline-icon nil)                     ;; Disable icons if nerd fonts are not being used.
  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-position-column-line-format '("%l:%c"))

  :hook
  (elpaca-after-init . doom-modeline-mode))

(provide 'init-doom-modeline)
;;; init-doom-modeline.el ends here.
