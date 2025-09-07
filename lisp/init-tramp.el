;;; init-tramp.el --- optimizations for tramp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)
(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)
(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(remove-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
(remove-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(remove-hook 'find-file-hook 'forge-bug-reference-setup)

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(connection-local-set-profile-variables
 'my-auto-save-profile
 '((buffer-auto-save-file-name . nil)))

(connection-local-set-profiles
 '(:application tramp :protocol "sudo")
 'my-auto-save-profile)
;; More aggressive approach - disable all nerd-icons functions for remote files
(defadvice nerd-icons-dired-mode (around disable-for-tramp activate)
  "Don't enable nerd-icons-dired-mode for TRAMP buffers."
  (unless (file-remote-p default-directory)
    ad-do-it))

(provide 'init-tramp)
;;; init-tramp.el ends here.
