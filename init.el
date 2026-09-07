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
(require 'init-cape)
(require 'init-diff-hl)
(require 'init-dired)
(require 'init-doom-modeline)
(require 'init-dotenv)
(require 'init-eglot)
(require 'init-eldoc-box)
(require 'init-eldoc)
(require 'init-emacs-options)
(require 'init-embark-consult)
(require 'init-embark)
;;(require 'init-evil)
(require 'init-flymake)
(require 'init-helpful)
(require 'init-isearch)
(require 'init-nerd-icons)
(require 'init-magit)
(require 'init-ediff)
(require 'init-marginalia)
(require 'init-markdown)
(require 'init-dape)
(require 'init-nix)
(require 'init-go)
(require 'init-orderless)
(require 'init-denote)
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
(require 'init-custom-func)
(require 'init-meow)
(require 'init-org)


;; this has to be last (i actually dunno)
;; (require 'init-keybinds-general)

;; (use-package vterm
;;   :ensure t
;;   :config
;;   (advice-add 'meow-kill :around (lambda (orig-fun &rest args)
;;                                    (if (derived-mode-p 'vterm-mode)
;;                                        (if (region-active-p)
;;                                            (vterm-delete-region
;;                                             (region-beginning) (region-end))
;;                                          (vterm-delete-region
;;                                           (point) (pos-eol)))
;;                                      (apply orig-fun args))))
;;   :hook
;;   (vterm-mode . (lambda ()
;;                   (add-hook 'meow-insert-enter-hook
;;                             (lambda () (vterm-copy-mode -1)) nil t)
;;                   (add-hook 'meow-insert-exit-hook 'vterm-copy-mode nil t)))
;;   )

(use-package ghostel
  :ensure t
  :custom
  (ghostel-keymap-exceptions
   '("C-c" "C-x" "C-u" "C-h" "M-x" "C-M-g" "M-:" "C-\\"
     "C-v" "M-v" "C-r" "C-s")))

(use-package meow-ghostel
  :vc (:url "https://github.com/dakra/meow-ghostel"
       :rev :newest)
  :after (ghostel meow)
  :hook (ghostel-mode . meow-ghostel-mode))

(use-package jinx
  :ensure t)

;; Extra packages which don't require any configuration
(use-package sudo-edit
  :ensure t)

(use-package avy
  :ensure t)

(use-package gt
  :ensure t
  :config
  (setq gt-langs '(en de))
  (setq gt-default-translator
        (gt-translator :engines (gt-google-engine))))

(use-package wgrep
  :ensure t)

(use-package neocaml
  :ensure (:host github
           :repo "bbatsov/neocaml"
           :depth treeless))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package envrc
  :ensure t
  :hook
  (elpaca-after-init . envrc-global-mode))

(use-package kdl-mode
  :ensure t
  :defer t)

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

(use-package sicp
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package clojure-ts-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t)

(use-package inf-clojure
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package just-mode
  :ensure t
  :defer t)

(use-package gptel
  :ensure (:host github
           :repo "karthink/gptel"
           :branch "master"
           :files ("*.el"))
  :config
  (setq gptel-backend
        (gptel-make-openai-oauth "OpenAI-sub"))

  (setq gptel-model 'gpt-5.6-sol))

(use-package rego-mode
  :ensure t
  :mode "\\.rego\\'"
  :custom
  (rego-repl-executable "opa")
  (rego-opa-command "opa"))

(use-package ox-hugo
  :ensure t   
  :after ox)

(use-package zig-mode
  :ensure t)

(use-package ox-pandoc
  :ensure t   
  :after ox)

(use-package uuidgen
  :ensure t)


(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(defun my/optimize-reading-layout ()
  "Setup line spacing for a better reading experience."
  (setq line-spacing 0.25)) ;; 0.25 = 25% extra height between lines

(use-package mixed-pitch
  :ensure t
  :hook
  (Info-mode . mixed-pitch-mode)
  (Info-mode . my/optimize-reading-layout)
  :config
  (set-face-attribute 'variable-pitch nil :font "ET Book" :height 200))

(use-package rainbow-mode
  :ensure t
  :defer t
  :init
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'text-mode-hook 'rainbow-mode)
)

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

(use-package highlight-indent-guides
  :ensure t
  :config
  ;; Disable auto-color calculation (it fails with Rose Pine)
  (setq highlight-indent-guides-auto-enabled nil)

  ;; Set the guide characters to "Highlight Med" (subtle grey-purple)
  (set-face-foreground 'highlight-indent-guides-character-face "#403d52")
  (set-face-foreground 'highlight-indent-guides-odd-face "#403d52")
  (set-face-foreground 'highlight-indent-guides-even-face "#403d52")

  ;; Set the "active" guide (where your cursor is) to "Iris" (purple)
  ;; Note: This requires 'highlight-indent-guides-responsive' to be set to 'top
  (set-face-foreground 'highlight-indent-guides-top-character-face "#c4a7e7")
  (set-face-foreground 'highlight-indent-guides-top-odd-face "#c4a7e7")
  (set-face-foreground 'highlight-indent-guides-top-even-face "#c4a7e7")
  (set-face-foreground 'highlight-indent-guides-stack-character-face "#c4a7e7")
  (set-face-foreground 'highlight-indent-guides-stack-odd-face "#c4a7e7")
  (set-face-foreground 'highlight-indent-guides-stack-even-face "#c4a7e7")

  :custom
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0))

(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)
(setq typescript-ts-mode-indent-offset 2)
(setq tsx-ts-mode-indent-offset 2)

(provide 'init)

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
