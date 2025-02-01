;; Basic Options
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))


(setq inhibit-startup-message t
      blink-cursor-mode nil
      custom-file "~/.emacs.d/custom.el"
      tool-bar-mode -1
      vc-follow-symlinks t
      scroll-bar-mode -1
      tooltip-mode -1
      global-display-line-numbers-mode t
      column-number-mode t
      frame-resize-pixelwise t
      display-line-numbers 'relative)
(setq default-frame-alist '((fullscreen . maximized)


                            ;; You can turn off scroll bars by uncommenting these lines:
                             (vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
(add-hook 'text-mode-hook 'visual-line-mode)

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)
(pixel-scroll-precision-mode)                         ; Smooth scrolling

(set-face-attribute 'default nil :font "JetBrains Mono Nerd Font" :height 110 )

(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
;; Initialize package system
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
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)


;; Enable vertico
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 15) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
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

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
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
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
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



;; Corfu - Completion UI
(use-package corfu
  :init
  (global-corfu-mode))

;; Cape - Completion Extensions
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Generale
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer leader
    :keymaps '(normal visual emacs )
    :prefix "SPC"
    :global-prefix "S-SPC"))

(leader
  "SPC" '(projectile-find-file :which-key "project ff")
  "," '(switch-to-buffer :which-key "find buffer")
  "." '(find-file-at-point :which-key "ff in dir")
  "TAB" '(perspective-map :which-key "perspective")

  "c" '(:igore t :which-key "code")
  "cc" '(eglot-code-action :which-key "code actions")
  "cf" '(eglot-format-buffer :which-key "format buffer")
  "cr" '(eglot-rename :which-key "rename")
  "co" '(eglot-organize-imports :which-key "organize imports")

  "e" '(:ignore t :which-key "eval elisp")
  "eb" '(eval-buffer :which-key "eval buffer")
  "er" '(eval-region :which-key "eval region")
  "ee" '(eval-expression :which-key "eval expression")
  "ed" '(eval-expression :which-key "eval expression")

  "f" '(:ignore f :which-key "file")
  "ff" '(find-file :which-key "find file")

  "g" '(:ignore g :which-key "git")
  "gg" '(magit-status :which-key "magit")

  "o" '(:igore o :which-key "open")
  "od" '(dired-jump :which-key "open dired")
  "ot" '(vterm :which-key "open terminal")


  "n" '(:ignore :which-key "nodes")
  "nf" '(org-roam-node-find :which-key "find node")
  "ng" '(org-roam-graph :which-key "show graph")
  "ni" '(org-roam-node-insert :which-key "insert node")
  "nc" '(org-roam-capture :which-key "capture node")
  "nj" '(org-roam-dailies-capture-today :which-key "capture daily node") 

  "p" '(projectile-command-map p :which-key "project")

  "qq" '(evil-quit :which-key "quit emcas")

  "s" '(:ignore :which-key "spelling")
  "ss" '(jinx-correct :which-key "jinx correct")
  "sl" '(jinx-languages :which-key "set language")
  "sn" '(jinx-correct-next :which-key "jinx next")
  "sN" '(jinx-correct-previous :which-key "jinx previous")
  "sa" '(jinx-correct-all :which-key "jinx corect all")

  "t"  '(:ignore t :which-key "toggles")
  "tt" '(multi-vterm-dedicated-toggle :which-key "Toggle dedicated terminal")
  "tT" '(multi-vterm-project :which-key "Project-based terminal")
  "tn" '(multi-vterm-next :which-key "Switch to next terminal")
  "tp" '(multi-vterm-prev :which-key "Switch to previous terminal")

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
 "gd" 'eglot-find-declaration
 "gD" 'eglot-find-typeDefinition
 "gi" 'eglot-find-implementation
 "gr" 'xref-find-references
 ;;"K"  'eldoc
 )

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "h" 'dired-up-directory
 "l" 'dired-find-alternate-file)

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
;; Marginalia - Rich Annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Evil - Vim Emulation
(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-C-u-scroll t
	evil-undo-system 'undo-fu
        evil-want-keybinding nil) ;; Disable default )
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package undo-fu)

;; Evil Collection - Integration for Evil Mode
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Set Leader Key for Evil
(evil-define-key 'motion global-map (kbd "C-u") 'evil-scroll-up)

;; Gruber Darker Theme
(use-package gruber-darker-theme
  :ensure t)

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

;;;;; SET Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;(load-theme 'gruber-darker t)
(load-theme 'doom-rose-pine t)

;; Projectile - Project Management
(use-package projectile
  :ensure t
  :init
  :config
  (projectile-mode +1))
;; Which-Key - Keybinding Help
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-position-line-format '(":%l")
      ;; for some reason this does not work
      doom-modeline-position-column-line-format '("%l:%c")
      doom-modeline-total-line-number t)

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

(use-package olivetti)

(use-package magit)

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

(use-package jinx
  :hook (emacs-startup . global-jinx-mode))

;;;; Treeitter and Eglot LSP ;;;;;;;;
(use-package tree-sitter-langs)
(setq global-tree-sitter-mode t)
(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))


(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)
(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )


(put 'dired-find-alternate-file 'disabled nil)
