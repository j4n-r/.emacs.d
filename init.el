;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'init-elpaca)

(setq jit-lock-defer-time 0)

(setq auth-sources '("~/.authinfo"))

(require 'init-consult)
(require 'init-corfu)
(require 'init-diff-hl)
(require 'init-dired)
(require 'init-doom-modeline)
(require 'init-dotenv)
(require 'init-eglot)
(require 'init-eldoc-box)
(require 'init-eldoc)
(require 'init-elpaca)
(require 'init-emacs-options)
(require 'init-embark-consult)
(require 'init-embark)
(require 'init-evil)
(require 'init-flymake)
(require 'init-helpful)
(require 'init-isearch)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-markdown)
(require 'init-dape)
(require 'init-nerd-icons)
(require 'init-nix)
(require 'init-elixir)
(require 'init-go)
(require 'init-zig)
(require 'init-orderless)
(require 'init-org-roam)
(require 'init-denote)
(require 'init-proced)
;; (require 'init-pulsar)
(require 'init-themes)
(require 'init-tramp)
(require 'init-treesit-auto)
(require 'init-typst)
(require 'init-undo-tree)
(require 'init-vertico)
(require 'init-which-key)
(require 'init-window)
(require 'init-xclip)
(require 'init-yasnippet)
(require 'init-perspective)
(require 'init-hl-todo)
(require 'init-pdf-tools)

;; this has to be last (i actually dunno)
(require 'init-keybinds-general)

(use-package vterm
    :ensure t)

;; Extra packages which don't require any configuration
(use-package sudo-edit
  :ensure t)

(use-package avy
  :ensure t)

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package envrc
  :ensure t
  :hook
  (elpaca-after-init . envrc-global-mode))

(use-package ace-window
  :ensure t
  :custom
  ;(aw-keys '(?f ?d ?s ?v ?c ?x)) ;; 123456 without modifier key
  (aw-background nil))

(use-package docker
  :ensure t
  :defer t)

(use-package realgud
  :ensure t
  :defer t)

(use-package olivetti
  :ensure t
  :defer t)

(use-package org-download
  :ensure t
  :defer t)

(use-package nov
  :ensure t
  :defer t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
)


(setq c-ts-mode-indent-offset 4)
(setq typescript-ts-mode-indent-offset 2)
(setq tsx-ts-mode-indent-offset 2)

(provide 'init)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
