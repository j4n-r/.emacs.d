;;; emacs-options-window.el --- Configure emacs internal options  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package emacs
  :ensure nil

  ;; Better to keep simple variables under :custom
  :custom
  (column-number-mode t)
  (auto-save-default nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 0)
  (split-height-threshold nil)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)
  (scroll-margin 10)
  (scroll-step 1)
  (org-duration-format 'h:mm)
  (blink-cursor-mode 'nil)
  (fill-column 80)
  (reb-re-syntax 'string)

  :hook
  (prog-mode . display-line-numbers-mode)
  ;; (shell-command-mode . special-mode)

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (global-hl-line-mode -1)
  (global-auto-revert-mode 1)
  (setq-default indent-tabs-mode nil)   ;; buffer-local → set default
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode 1)
  (global-visual-line-mode 1)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1)
  (modify-coding-system-alist 'file "" 'utf-8)
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (message "Emacs has fully loaded. This code runs after startup.")
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                                (emacs-init-time)
                                (number-to-string (length package-activated-list)))))))

  :config
  ;; Skip special buffers when cycling
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Fonts (safe even in TTY; no-op if font isn't available)
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 155)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 135))

  ;; Separate customizations file
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Vertical window divider: use U+2502 BOX DRAWINGS LIGHT VERTICAL
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  ;; Modeline: battery + time (use setq for variables, then enable the mode)
  (display-battery-mode 1)
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-interval 30
        display-time-use-mail-icon nil
        display-time-format "%d.%m.%y %H:%M "
        display-time-default-load-average nil)
  (display-time-mode 1))

(provide 'init-emacs-options)
;;; init-emacs-options.el ends here.
