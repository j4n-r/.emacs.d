;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           EMACS OPTIONS               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq read-process-output-max (* 10 1024 1024) ;; 10mb
      gc-cons-threshold 200000000
      enable-recursive-minibuffers t)

(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent
(setq auth-sources '("~/.authinfo"))
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         PACKAGE MANAGEMENT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

(use-package emacs
  :custom
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  ;;(text-mode-ispell-word-completion nil)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (recentf-mode t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (ring-bell-function 'ignore)
  (scroll-step 1)
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (inhibit-startup-message t)                     ;; Disable the startup message when Emacs launches.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (scroll-margin 10)
  (create-lockfiles nil)   ; No backup files
  (make-backup-files nil)  ; No backup files
  (backup-inhibited t)     ; No backup files
  (auto-save-default nil) ; stop creating #autosave# files
  (save-place-mode t)
  (org-duration-format 'h:mm)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p))
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Set a directory for auto-save files
(defvar my-auto-save-folder (expand-file-name "~/.emacs.d/autosaves/"))
(unless (file-exists-p my-auto-save-folder)
  (make-directory my-auto-save-folder t))

(setq auto-save-file-name-transforms
      `((".*" ,my-auto-save-folder t)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs has fully loaded. This code runs after startup.")

              ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format
                         ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                         (emacs-init-time)
                         (number-to-string (length package-activated-list))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI STUFF                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))


;; Gruber Darker Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'doom-rose-pine t)
(setq inhibit-startup-message t
      blink-cursor-mode nil
      custom-file "~/.emacs.d/custom.el"
      tool-bar-mode nil
      vc-follow-symlinks t
      scroll-bar-mode nil
      tooltip-mode nil
      global-display-line-numbers-mode t
      column-number-mode t
      frame-resize-pixelwise t
	  global-hl-line-mode t
	  global-auto-revert-mode t
      display-line-numbers 'relative)


(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(setq-default indent-tabs-mode nil  ;; Use spaces instead of tabs
              tab-width 4)           ;; Set default tab width to 4

(menu-bar--display-line-numbers-mode-relative)
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
(add-hook 'text-mode-hook 'visual-line-mode)

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 110)
(when (eq system-type 'darwin)       ;; Check if the system is macOS.
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 145))



( display-battery-mode )
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 30)
(setq display-time-use-mail-icon nil)
(setq display-time-format "%d.%m.%y %H:%M")
(setq display-time-default-load-average nil)
(display-time-mode 1)


;; (use-package pulsar
;;   :defer t
;;   :straight t
;;   :ensure t
;;   :hook
;;   (after-init . pulsar-global-mode)
;;   :config
;;   (setq pulsar-pulse t)
;;   (setq pulsar-delay 0.035)
;;   (setq pulsar-iterations 5)
;;   (setq pulsar-face 'evil-ex-lazy-highlight)

;;   (add-to-list 'pulsar-pulse-functions 'evil-yank)
;;   (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
;;   (add-to-list 'pulsar-pulse-functions 'evil-delete)
;;   (add-to-list 'pulsar-pulse-functions 'evil-delete-line))


(use-package nerd-icons
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t)                               ;; Load the package only when needed to improve startup time.

(use-package nerd-icons-dired
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t                                ;; Load the package only when needed to improve startup time.
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :after (:all nerd-icons marginalia)     ;; Load after `nerd-icons' and `marginalia' to ensure proper integration.
  :config
  (nerd-icons-completion-mode)            ;; Activate nerd icons for completion interfaces.
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)) ;; Setup icons in the marginalia mode for enhanced completion display.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             ESSENTIALS                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package eldoc
  :ensure nil          ;; This is built-in, no need to fetch it.
  :init
  (global-eldoc-mode))

;; Vertical minibuffer interface
(use-package vertico
  :custom
  (vertico-count 15) ;; Show more candidates
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; search and navigation  
(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;;(setq consult-narrow-key "<") ;; "C-+"
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;;(keymap-set consult-narrow-map (concat consult-narrow-key "?") #'consult-narrow-help)
  )
;; better matching (searching)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; popup UI
(use-package corfu
  ;; Optional customizations
  :custom
  ;;(corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-auto-delay 0)            ; No delay for completion

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))


;; Minibuffer Annotations
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Context sensitive commands
(use-package embark)
(use-package embark-consult)

;; Add Completion at Point sources
(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-<tab>" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  )


;; Helpful
(use-package helpful)
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)


;; Projectile - Project Management
(use-package projectile
  :ensure t
  :init
  :config
  (projectile-mode +1))
;; shit solution
(setq projectile-git-submodule-command nil)

;; Which-Key - Keybinding Help
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package all-the-icons)
(use-package avy)

(use-package undo-tree
  :defer t
  :ensure t
  :straight t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        ;; These values can be adjusted based on your needs.
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncates the undo history very aggressively.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            MISCELLANEOUS               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package proced
  :ensure nil
  :commands proced
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-sort 'pmem)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid  pcpu pmem rss start time state (args comm))))

;; Doom Modeline configuration
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-position-line-format '(":%l")
        ;; for some reason this does not work
        doom-modeline-position-column-line-format '("%l:%c")
        doom-modeline-total-line-number t))

(use-package vterm)
(use-package multi-vterm)
(setq multi-vterm-dedicated-window-height-percent 30)

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")) 
  :init
  (persp-mode))

(use-package leetcode
  :custom
  leetcode-prefer-language "rust"
  leetcode-prefer-sql "postgresql"
  leetcode-save-solutions t
  leetcode-directory "~/dev/leetcode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         DEV & LSP CONFIGURATION       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :custom
  (magit-clone-submodules 'recursive))

(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(use-package forge
  :after magit)

;; refresh buffers on source control change
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package apheleia
  :custom
  apheleia-global-mode +1)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)) 
(use-package yasnippet-snippets)


(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (kotlin-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (python-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp)
         (c-ts-mode . lsp)
         (css-ts-mode . lsp)        
         (html-ts-mode . lsp)      
         (c-ts-mode .lsp)
         (c++-ts-mode .lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-inlay-hint-enable t)                             ;; Enable inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0)                                    ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color nil)                  ;; Disable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flymake' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all nil)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable t)                                   ;; Enable lens support.
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)   ;; Enable symbol numbers in the headerline.
  (lsp-headerline-arrow "▶")                            ;; Set arrow for headerline.
  (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics in headerline.
  (lsp-headerline-breadcrumb-icons-enable nil)          ;; Disable icons in breadcrumb.
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil)                     ;; Disable semantic tokens.
  ;; :custom
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints nil)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)
  )

(use-package rustic)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-side 'right))
(use-package consult-lsp :commands consult-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

;;;; Python ;;;;;
(use-package lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  ( lsp-pyright-python-executable-cmd (executable-find "python"))
  :hook (python-ts-mode . lsp-deferred))
(setf (alist-get 'python-ts-mode apheleia-mode-alist)
      '(ruff-isort ruff))

;; no automatic venv activation since managed with nix flake
(use-package pyvenv
  :config
  (pyvenv-mode 0))


(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; (use-package direnv
;;   :config
;;   (direnv-mode))

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(use-package cmake-mode)

;; (add-to-list 'lsp-language-id-configuration '(html-mode . "html"))
;; (add-to-list 'lsp-language-id-configuration '(css-mode . "css"))
;; (add-to-list 'lsp-language-id-configuration '(tsx-ts-mode . "tsx"))

(use-package lsp-tailwindcss
  :ensure t
  :straight t
  :defer t
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html")) ;; Associate ERB files with HTML.
  :init
  (setq lsp-tailwindcss-add-on-mode t))



(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))

;; code folding
(use-package dash)
(use-package s)
(use-package origami
  :config
  (global-origami-mode t))



(use-package kotlin-mode)

(use-package pg :vc (:url "https://github.com/emarsden/pg-el/"))
(use-package pgmacs :vc (:url "https://github.com/emarsden/pgmacs/"))
(setq sql-sqlite-program "sqlite3")
(setq sql-postgres-program "/run/current-system/sw/bin/psql")


(defun get-gemini-key ()
  "Retrieve the password from the first entry in .authinfo for generativelanguage.googleapis.com.
Returns the password string, or nil if no matching entry is found.

~/.authinfo
machine generativelanguage.googleapis.com login apikey password {key}
"
  (let ((entry (car (auth-source-search :host "generativelanguage.googleapis.com" :max 1))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun get-claude-key ()
  "Retrieve the password from the first entry in .authinfo for generativelanguage.googleapis.com.
Returns the password string, or nil if no matching entry is found.

~/.authinfo
 machine api.anthropic.com login apikey password {key}
"
  (let ((entry (car (auth-source-search :host "api.anthropic.com" :max 1))))
    (when entry
      (let ((secret (plist-get entry :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(use-package gptel)
(setq
 gptel-model 'gemini-2.5-pro-preview-05-06
 gptel-backend (gptel-make-gemini "Gemini"
                 :key (get-gemini-key)
                 :stream t))
(setq gptel-backend (gptel-make-anthropic "Claude"          
                      :stream t                             
                      :key (get-claude-key)))
(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

(use-package aidermacs
  :config
  (setenv "GEMINI_API_KEY" (get-gemini-key))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "gemini"))

(use-package verb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ORG MODE & NOTES               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package olivetti)
(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 100))) 

;; Must do this so the agenda knows where to look for my files
(setq org-agenda-files '("~/org-roam"))

;; When a TODO is set to a done state, record a timestamp
(setq org-log-done 'time)

;; Follow the links
(setq org-return-follows-link  t)

;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make the indentation look nicer
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; (use-package jinx
;;   :hook (emacs-startup . global-jinx-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

(use-package tablist)
(use-package pdf-tools)
(pdf-tools-install)  ; Standard activation command
(pdf-loader-install) ; On demand loading, leads to faster startup time

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "comrak"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            KEYBINDINGS                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generale
(use-package general
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
  (general-create-definer leader
    :keymaps '(normal visual emacs )
    :prefix "SPC"
    :global-prefix "S-SPC"))
(leader
  "SPC" '(projectile-find-file :which-key "project ff")
  "," '(consult-buffer :which-key "find buffer")
  "." '(find-file-at-point :which-key "ff in dir")
  "TAB" '(perspective-map :which-key "perspective")
  "TAB TAB" '(persp-switch :which-key "perspective")

  "a" '(aidermacs-transient-menu :which-key "aider")

  "b" '(:ignore :which-key "buffer")
  "br" '(rename-buffer :which-key "rename buffer")
  "bk" '(kill-buffer :which-key "kill buffer")

  "c" '(:igore t :which-key "code")
  "ca" '(lsp-execute-code-action :which-key "code actions")
  "cc" '(compile :which-key "compile")
  "cf" '(lsp-format-buffer :which-key "format buffer")
  "cF" '(apheleia-format-buffer :which-key "format buffer")
  "cr" '(lsp-rename :which-key "rename")
  "co" '(lsp-organize-imports :which-key "organize imports")

  "e" '(:ignore t :which-key "eval elisp")
  "eb" '(eval-buffer :which-key "eval buffer")
  "er" '(eval-region :which-key "eval region")
  "ee" '(eval-expression :which-key "eval expression")
  "ed" '(eval-expression :which-key "eval expression")

  "f" '(:ignore t :which-key "file")
  "ff" '(find-file :which-key "find file")
  "fr" '(rename-visited-file :which-key "rename file")

  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "magit")
  "gb" '(magit-blame  :which-key "magit blame")
  "gd" '(magit-diff-buffer-file  :which-key "magit diff")
  "gr" '(magit-refresh-buffer  :which-key "magit refresh buffer")

  "gt" '(:ignore t :which-key "gptel")
  "gts" '(gptel-send            :which-key "Send Query")
  "gtn" '(gptel                 :which-key "New Chat Buffer")
  "gtr" '(gptel-rewrite         :which-key "Rewrite/Refactor")
  "gta" '(gptel-add             :which-key "Add Context")
  "gtf" '(gptel-add-file        :which-key "Add File to Context")
  "gto" '(gptel-org-set-topic   :which-key "Set Org Topic")
  "gtp" '(gptel-org-set-properties :which-key "Set Org Properties")
  "gtm" '(gptel-menu :which-key "Set Org Properties")

  "o" '(:igore t :which-key "open/org")
  "od" '(dired-jump :which-key "open dired")
  "ot" '(multi-vterm-project :which-key "open project terminal")
  "oT" '(vterm :which-key "open terminal")
  "op" '(proced :which-key "open proced")
  "oc" '(:ignore t :which-key "org clock")
  "oci" '(org-clock-in :which-key "clock in")
  "oco" '(org-clock-out :which-key "clock out")
  "ocr" '(org-clock-report :which-key "clock report")
  "oca"  '(org-clock-goto :which-key "goto active clock")
  "obu" '(org-dblock-update :which-key "org block update")
  "obU" '(org-update-all-dblocks :which-key "org block update all")


  "l" '(:ignore t :which-key "lsp/linter")
  "ln" '(flycheck-next-error :which-key "lint next error")
  "lN" '(flycheck-previous-error :which-key "lint previous error")

  "n" '(:ignore :which-key "nodes")
  "nf" '(org-roam-node-find :which-key "find node")
  "ng" '(org-roam-graph :which-key "show graph")
  "ni" '(org-roam-node-insert :which-key "insert node")
  "nc" '(org-roam-capture :which-key "capture node")
  "nj" '(org-roam-dailies-capture-today :which-key "capture daily node") 

  "p" '(projectile-command-map p :which-key "project")

  "qr" '(query-replace :which-key "Query Replace")
  "qR" '(query-replace-regexp :which-key "Query regex Replace")
  "qq" '(evil-quit :which-key "quit emcas")

  "r" '(verb-command-map :which-key "restclient")

  "s" '(:ignore :which-key "spelling/search")
  "sc" '(jinx-correct :which-key "correct spelling")
  "sl" '(jinx-languages :which-key "set language")
  "sn" '(jinx-correct-next :which-key "jinx next")
  "sN" '(jinx-correct-previous :which-key "jinx previous")
  "sa" '(jinx-correct-all :which-key "jinx corect all")
  "sg" '(consult-ripgrep :which-key "search grep")
  "sm" '(consult-ripgrep :which-key "search manpages")
  "ss" '(consult-lsp-file-symbols :which-key "workspace symbol")
  "sS" '(consult-lsp-workspace-symbol :which-key "workspace symbol")

  "t"  '(:ignore t :which-key "toggles")

  "w" '(:ignore w :which-key "window")
  "wv" '(evil-window-vsplit :which-key "split vertically")
  "ws" '(evil-window-split :which-key "split horizontally")
  "wq" '(delete-window :which-key "delete window")

  "wo" '(:ignore t :which-key "olivetti")
  "woo" '(olivetti-mode :which-key "center")
  "wo>" '(olivetti-expand :which-key "expand margin")
  "wo<" '(olivetti-shrink :which-key "shrink margin")
  "wos" '(olivetti-set-width :which-key "set width")

  "ww" '(other-window :which-key "switch window")
  "w." '(balance-windows :which-key "switch window")
  "w<" '(evil-window-decrease-width :which-key "decrease width")
  "w>" '(evil-window-increase-width :which-key "increase width")
  "w+" '(evil-window-increase-height :which-key "increase height")
  "w-" '(evil-window-decrease-height :which-key "decrease height")
  "wh" '(evil-window-left :which-key "window left")
  "wl" '(evil-window-right :which-key "window right")
  "wj" '(evil-window-down :which-key "window down")
  "wk" '(evil-window-up :which-key "window up")
  "wH" '(evil-window-move-far-left :which-key "move window far left")
  "wL" '(evil-window-move-far-right :which-key "move window far right")
  "wJ" '(evil-window-move-very-bottom :which-key "move window very bottom")
  "wK" '(evil-window-move-very-top :which-key "move window very top")
  "wt" '(evil-window-top-left :which-key "top-left window")
  "wb" '(evil-window-bottom-right :which-key "bottom-right window")
  "w=" '(balance-windows :which-key "balance windows")
  "wr" '(evil-window-rotate-downwards :which-key "rotate downwards")
  "wR" '(evil-window-rotate-upwards :which-key "rotate upwards")
  )

(general-define-key
 :states 'normal
 "gd"  'lsp-find-definition   
 "gr"  'lsp-find-references    
 "gI"  'lsp-find-implementation 
 "gy"  'lsp-find-type-definition 
 "gD"  'lsp-find-declaration      
 "K"   'lsp-ui-doc-glance          
 "J" 'lsp-ui-doc-focus-frame
 "H" 'lsp-ui-doc-hide 
 "gcc" '(lambda ()
          (interactive)
          (if (not (use-region-p))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
 )

(general-define-key
 :states 'motion
 
 "gc" 'comment-or-uncomment-region)

(general-define-key
 "s-j" 'avy-goto-char-2
 "M-e" 'embark-act)

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "n" 'dired-create-empty-file
 "N" 'dired-create-directory
 "h" 'dired-up-directory
 "l" 'dired-find-alternate-file
 )


(defun my-dired-unbind-spc ()
  "Unbind SPC in dired-mode's normal state map."
  (general-unbind :keymaps  'dired-mode-map :states 'normal "SPC"))
(add-hook 'dired-mode-hook #'my-dired-unbind-spc)

(defun my-leetcode-unbind-spc ()
  (general-unbind :keymaps 'leetcode--problem-detail-mode-map :states 'normal "SPC"))
(add-hook 'leetcode--problems-mode-hook #'my-leetcode-unbind-spc)

(general-define-key
 :states 'normal
 :keymaps 'magit-mode-map
 "N" nil)

(defun jinx-correct-next ()
  "Move to next incorrect word and correct it"
  (interactive)
  (jinx-next 1)
  (jinx-correct))

(defun jinx-correct-previous ()
  "Move to previous incorrect word and correct it"
  (interactive)
  (jinx-previous 1)
  (jinx-correct))

;; Evil - Vim Emulation
(use-package evil
  :init
  (setq evil-want-integration t
        ;;evil-want-minibuffer t
	    evil-want-C-u-scroll t
	    evil-undo-system 'undo-tree
        evil-want-keybinding nil) ;; Disable default )
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; (use-package undo-fu)

;; Evil Collection - Integration for Evil Mode
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :straight t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(put 'dired-find-alternate-file 'disabled nil)
