;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
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
;; (require 'init-utils)
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpaca)      ;; Machinery for installing required packages

(setq jit-lock-defer-time 0)

;; Allow users to provide an optional "init-preload-local.el"
 (require 'init-consult)
(require 'init-corfu)
(require 'init-diff-hl)
(require 'init-dired)
(require 'init-doom-modeline)
(require 'init-dotenv)
(require 'init-eglot)
;;(require 'init-eldoc-box)
(require 'init-eldoc)
(require 'init-elpaca)
(require 'init-emacs-options)
(require 'init-embark-consult)
(require 'init-embark)
(require 'init-evil)
(require 'init-flymake)
(require 'init-helpful)
(require 'init-isearch)
(require 'init-keybinds-general)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-markdown)
(require 'init-nerd-icons)
(require 'init-nix)
(require 'init-orderless)
(require 'init-org-roam)
(require 'init-proced)
(require 'init-pulsar)
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


;; Extra packages which don't require any configuration
(use-package sudo-edit
  :ensure t)
(use-package avy
  :ensure t)
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(provide 'init)

;;; init.el ends here
