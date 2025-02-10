;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           EMACS OPTIONS               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq read-process-output-max (* 10 1024 1024) ;; 10mb
      gc-cons-threshold 200000000
      enable-recursive-minibuffers t)

;; Don't litter file system with *~ backup files; put them all inside ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

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
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

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
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI STUFF                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

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
      display-line-numbers 'relative)

(setq-default indent-tabs-mode nil  ;; Use spaces instead of tabs
              tab-width 2)           ;; Set default tab width to 4

(menu-bar--display-line-numbers-mode-relative)
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
(add-hook 'text-mode-hook 'visual-line-mode)

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 150 ); 150

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             ESSENTIALS                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Which-Key - Keybinding Help
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            MISCELLANEOUS               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         DEV & LSP CONFIGURATION       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit)
;; refresh buffers on source control change
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(use-package apheleia
  :custom
  apheleia-global-mode +1)

(use-package yasnippet-snippets)


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package consult-lsp :commands consult-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))


(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))

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

(use-package jinx
  :hook (emacs-startup . global-jinx-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            KEYBINDINGS                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generale
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer leader
    :keymaps '(normal visual emacs )
    :prefix "SPC"
    :global-prefix "S-SPC"))
(setq lsp-keymap-prefix "C-c l")
(leader
  "SPC" '(projectile-find-file :which-key "project ff")
  "," '(switch-to-buffer :which-key "find buffer")
  "." '(find-file-at-point :which-key "ff in dir")
  "TAB" '(perspective-map :which-key "perspective")

  "c" '(:igore t :which-key "code")
  "ca" '(lsp-execute-code-action :which-key "code actions")
  "cc" '(compile :which-key "compile")
  "cf" '(lsp-format :which-key "format buffer")
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

  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "magit")
  "gb" '(magit-blame  :which-key "magit blame")

  "o" '(:igore t :which-key "open")
  "od" '(dired-jump :which-key "open dired")
  "ot" '(vterm :which-key "open terminal")

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

  "qq" '(evil-quit :which-key "quit emcas")

  "s" '(:ignore :which-key "spelling")
  "ss" '(jinx-correct :which-key "correct spelling")
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
 :states 'motion
 "gc" 'comment-or-uncomment-region)

(general-define-key
 "M-e" 'embark-act)

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "n" 'dired-create-empty-file
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

