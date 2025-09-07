;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           EMACS OPTIONS               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq read-process-output-max (* 10 1024 1024) ;; 10mb
      gc-cons-threshold #x40000000)

(setq auth-sources '("~/.authinfo"))
(modify-coding-system-alist 'file "" 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         PACKAGE MANAGEMENT            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
;;(setq package-enable-at-startup nil) ;; Disables the default package manager.

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
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
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)

(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

;;; EMACS
(use-package emacs
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering in programming modes.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (inhibit-startup-message t)                     ;; Disable the startup message when Emacs launches.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.
  (scroll-margin 10)
  (scroll-step 1)
  (org-duration-format 'h:mm)
  ;; Modeline 
  (display-battery-mode )
  (display-time-24hr-format t)
  (display-time-day-and-date t)
  (display-time-interval 30)
  (display-time-use-mail-icon nil)
  (display-time-format "%d.%m.%y %H:%M ")
  (display-time-default-load-average nil)
  (display-time-mode 1)
  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.

  :config
  ;; By default emacs gives you access to a lot of *special* buffers, while navigating with [b and ]b,
  ;; this might be confusing for newcomers. This settings make sure ]b and [b will always load a
  ;; file buffer. To see all buffers use <leader> SPC, <leader> b l, or <leader> b i.
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)


  ;; Configure font settings based on the operating system.
  ;; Ok, this kickstart is meant to be used on the terminal, not on GUI.
  ;; But without this, I fear you could start Graphical Emacs and be sad :(
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 155)
  (when (eq system-type 'darwin)       ;; Check if the system is macOS.
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 135))

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  ;; You can M-x customize, M-x customize-group, or M-x customize-themes, etc.
  ;; The saves you do manually using the Emacs interface would overwrite this file.
  ;; The following makes sure those customizations are in a separate file.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.

  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.

  (global-hl-line-mode -1)     ;; Disable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Add a hook to run code after Emacs has fully initialized.
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


;;; TRAMP 
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              GUI STUFF                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WINDOW
;; This section configures window management in Emacs, enhancing the way buffers
;; are displayed for a more efficient workflow. The `window' use-package helps
;; streamline how various buffers are shown, especially those related to help,
;; diagnostics, and completion.
;;
;; Note: I have left some commented-out code below that may facilitate your
;; Emacs journey later on. These configurations can be useful for displaying
;; other types of buffers in side windows, allowing for a more organized workspace.
(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

;; This configuration customizes `dired' to enhance its usability. The settings
;; below specify how file listings are displayed, the target for file operations,
;; and associations for opening various file types with their respective applications.
;; For example, image files will open with `feh', while audio and video files
;; will utilize `mpv'.
(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))                     ;; Use GNU ls on macOS if available.
      (when gls
        (setq insert-directory-program gls)))))

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


(setq-default indent-tabs-mode nil  ;; Use spaces instead of tabs
              tab-width 4)           ;; Set default tab width to 4

(menu-bar--display-line-numbers-mode-relative)
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
(add-hook 'text-mode-hook 'visual-line-mode)

(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 155)
(when (eq system-type 'darwin)       ;; Check if the system is macOS.
  (setq insert-directory-program "/etc/profiles/per-user/jr/bin/gls")
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 135))



( display-battery-mode )
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 30)
(setq display-time-use-mail-icon nil)
(setq display-time-format "%d.%m.%y %H:%M ")
(setq display-time-default-load-average nil)
(display-time-mode 1)


;; (use-package pulsar
;;   :defer t
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
  :defer t)                               ;; Load the package only when needed to improve startup time.

(use-package nerd-icons-dired
  :defer t                                ;; Load the package only when needed to improve startup time.
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after (:all nerd-icons marginalia)     ;; Load after `nerd-icons' and `marginalia' to ensure proper integration.
  :config
  (nerd-icons-completion-mode)            ;; Activate nerd icons for completion interfaces.
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)) ;; Setup icons in the marginalia mode for enhanced completion display.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;             ESSENTIALS                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :config
  (setq eldoc-idle-delay 0)                  ;; Automatically fetch doc help
  (setq eldoc-echo-area-use-multiline-p nil) ;; We use the "K" floating help instead
  ;; set to t if you want docs on the echo area
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))

;;; FLYMAKE
;; Flymake is an on-the-fly syntax checking extension that provides real-time feedback
;; about errors and warnings in your code as you write. This can greatly enhance your
;; coding experience by catching issues early. The configuration below activates
;; Flymake mode in programming buffers.
(use-package flymake
  :ensure nil          ;; This is built-in, no need to fetch it.
  :defer t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error) (warning "»" compilation-warning)
     (note "»" compilation-info))))

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

(use-package xclip
  :defer t
  :hook
  (after-init . xclip-mode)) 

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


;; ;; Helpful
;; (use-package helpful)
;; ;; Note that the built-in `describe-function' includes both functions
;; ;; and macros. `helpful-function' is functions only, so we provide
;; ;; `helpful-callable' as a drop-in replacement.
;; (global-set-key (kbd "C-h f") #'helpful-callable)

;; (global-set-key (kbd "C-h v") #'helpful-variable)
;; (global-set-key (kbd "C-h k") #'helpful-key)
;; (global-set-key (kbd "C-h x") #'helpful-command)
;; ;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; ;; for this in lisp modes.
;; (global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; ;; Look up *F*unctions (excludes macros).
;; ;;
;; ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; ;; already links to the manual, if a function is referenced there.
;; (global-set-key (kbd "C-h F") #'helpful-function)


;; Which-Key - Keybinding Help
(use-package which-key
  :config
  (which-key-mode))


(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package all-the-icons)
(use-package avy)

(use-package undo-tree
  :defer t
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

(use-package sudo-edit)

(use-package harpoon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         DEV & LSP CONFIGURATION       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :custom
  (magit-clone-submodules 'recursive))

(use-package diff-hl
  :defer t
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
  :config
  (yas-global-mode 1)) 
(use-package yasnippet-snippets)


(add-hook 'kotlin-ts-mode 'eglot-ensure)
(add-hook 'rust-ts-mode  'eglot-ensure)
(add-hook 'python-ts-mode  'eglot-ensure)
(add-hook 'tsx-ts-mode  'eglot-ensure)
(add-hook 'typescript-ts-mode  'eglot-ensure)
(add-hook 'js-ts-mode  'eglot-ensure)
(add-hook 'c-ts-mode  'eglot-ensure)
(add-hook 'css-ts-mode  'eglot-ensure)        
(add-hook 'html-ts-mode  'eglot-ensure)      
(add-hook 'c++-ts-mode  'eglot-ensure)
(add-hook 'typst-ts-mode  'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(typst-ts-mode . ("tinymist" "--lsp"))
               '(nix-mode . ("nil" "--stdio"))))


;;; ELDOC-BOX
;; eldoc-box enhances the default Eldoc experience by displaying documentation in a popup box,
;; usually in a child frame. This makes it easier to read longer docstrings without relying on
;; the minibuffer. It integrates seamlessly with Eldoc and activates when Eldoc is active.
;; Useful for graphical Emacs; terminal users may want to fall back to `eldoc-box-display-at-point-mode'.
(use-package eldoc-box
  :defer t)


(setq c-ts-mode-indent-offset 4)
(use-package rustic)

(use-package typst-ts-mode
  :straight (:type git :host sourcehut :repo "meow_king/typst-ts-mode")
  :custom
  (typst-ts-mode-watch-options "--open"))

;; no automatic venv activation since managed with nix flake
(use-package pyvenv
  :config
  (pyvenv-mode 0))


(use-package nix-mode
  :hook (nix-mode . lsp-deferred))

(use-package envrc
  :config
  (envrc-global-mode))


(use-package cmake-mode)

;;(use-package lsp-tailwindcss
;;  :ensure t
;;  :defer t
;;  :config
;;  (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html")) ;; Associate ERB files with HTML.
;;  :init
;;  (setq lsp-tailwindcss-add-on-mode t))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))
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


(use-package pg :vc (:url "https://github.com/emarsden/pg-el/"))
(use-package pgmacs :vc (:url "https://github.com/emarsden/pgmacs/"))
(setq sql-sqlite-program "sqlite3")
(setq sql-postgres-program "/run/current-system/sw/bin/psql")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ORG MODE & NOTES               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (emacs-lisp . t)
   ))

(use-package olivetti)
(add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 100))) 

;; Must do this so the agenda knows where to look for my files
(setq org-agenda-files '("~/org-roam"))

;; When a TODO is set to a done state, record a timestamp
(setq org-log-done 'time)

;; Follow the links
(setq org-return-follows-link  t)

(setq org-startup-with-inline-images t)

;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make the indentation look nicer
(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package jinx)
;;(dolist (hook '(org-mode-hook)
;;            (add-hook hook #'jinx-mode)))
;; :hook (emacs-startup . global-jinx-mode))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

(use-package tablist)
(use-package pdf-tools)
(pdf-tools-install)  ; Standard activation command
(pdf-loader-install) ; On demand loading, leads to faster startup time

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "comrak"))

(use-package org-download
  :custom
  org-download-image-dir "~/org-roam/attachments"
  )
(use-package ox-hugo
  :config 
  :after ox)

;; Take the cursor to the post heading and using the default Shift+left (S-<left>) binding to mark that subtree as DONE.
;; Now save the file, take the cursor to the end of the post and type the bindings C-c C-e H H.
;; You should see the site preview in your browser auto-update!
;; Now as you make changes in your post, save and do C-c C-e H H to see the post update in the browser.

(setq org-hugo-default-section-directory "post")

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

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
    :prefix "SPC"))
;;:global-prefix "C-SPC"))
(leader
  "SPC" '(project-find-file :which-key "project ff")
  "," '(consult-buffer :which-key "find buffer")
  "." '(consult-project-buffer :which-key "find file under cursor")
  "TAB" '(perspective-map :which-key "perspective")
  "TAB TAB" '(persp-switch :which-key "perspective")


  "b" '(:ignore :which-key "buffer")
  "br" '(rename-buffer :which-key "rename buffer")
  "bk" '(kill-buffer :which-key "kill buffer")

  "c" '(:igore t :which-key "code")
  "ca" '(eglot-code-actions :which-key "code actions")
  "cc" '(compile :which-key "compile")
  "cf" '(eglot-format-buffer :which-key "format buffer")
  "cF" '(apheleia-format-buffer :which-key "format buffer")
  "cr" '(eglot-rename :which-key "rename")
  "co" '(eglot-code-action-organize-imports :which-key "organize imports")

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

  "n" '(:ignore :which-key "nodes")
  "nf" '(org-roam-node-find :which-key "find node")
  "ng" '(org-roam-graph :which-key "show graph")
  "ni" '(org-roam-node-insert :which-key "insert node")
  "nc" '(org-roam-capture :which-key "capture node")
  "nj" '(org-roam-dailies-capture-today :which-key "capture daily node") 

  "p" '(:ignore :which-key "project")
  "pp" '(project-switch-project :which-key "switch to project")
  "ps" '(project-search :which-key "switch to project")
  "ps" '(project-search :which-key "switch to project")

  "qr" '(query-replace :which-key "Query Replace")
  "qR" '(query-replace-regexp :which-key "Query regex Replace")
  "qq" '(evil-quit :which-key "quit emcas")

  "r" '(verb-command-map :which-key "restclient")

  "s" '(:ignore :which-key "spelling/search")
  "sc" '(jinx-correct :which-key "correct spelling")
  "sd" '(consult-lsp-diagnostics :which-key "lsp diagnostics")
  "sl" '(jinx-languages :which-key "set language")
  "sn" '(jinx-correct-next :which-key "jinx next")
  "sN" '(jinx-correct-previous :which-key "jinx previous")
  "sa" '(jinx-correct-all :which-key "jinx corect all")
  "sg" '(consult-ripgrep :which-key "search grep")
  "sm" '(consult-ripgrep :which-key "search manpages")
  "ss" '(consult-lsp-file-symbols :which-key "workspace symbol")
  "sS" '(consult-lsp-symbols :which-key "workspace symbol")

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
(leader
  :states 'normal
  :keymaps '(jtsx-jsx-mode-map jtsx-tsx-mode-map)
  "x" '(:ignore t :which-key "jsx/tsx")
  "xr" '(jtsx-rename-jsx-element :which-key "rename jsx element")
  "xw" '(jtsx-wrap-in-jsx-element :which-key "wrap in jsx element")
  "xu" '(jtsx-unwrap-jsx :which-key "unwrap jsx")
  "xD" '(jtsx-delete-jsx-node :which-key "delete jsx node")
  "xd" '(jtsx-delete-jsx-attribute :which-key "delete attribute")
  "xx" '(jtsx-jump-jsx-element-tag-dwim :which-key "jump jsx tag"))

(general-define-key
 :states 'normal
 "M-t" 'tab-to-tab-stop
 "M-i" 'consult-imenu
 "M-I" 'consult-imenu-multi
 "M-d" 'xref-find-definitions
 "M-r" 'xref-find-references
 "M-p" 'consult-yank-from-kill-ring
 "gd"  'lsp-find-definition   
 "gr"  'lsp-find-references    
 "gI"  'lsp-find-implementation 
 "gy"  'lsp-find-type-definition 
 "gD"  'lsp-find-declaration      
 "K"  'eldoc-box-help-at-point
 "gcc" '(lambda ()
          (interactive)
          (if (not (use-region-p))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
 )

(general-define-key
 :states 'motion
 "gc" 'comment-or-uncomment-region)

(general-define-key
 :states 'insert
 "M-t" 'tab-to-tab-stop)

(general-define-key
 :states 'motion
 :keymaps '(jtsx-jsx-mode-map jtsx-tsx-mode-map)
 "gc" 'jtsx-comment-dwim)


(general-define-key
 "s-j" 'avy-goto-char-2
 "M-e" 'embark-act)

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "f" 'find-file
 "n" 'dired-create-empty-file
 "N" 'dired-create-directory
 "h" 'dired-up-directory
 "l" 'dired-find-alternate-file
 ;; "M-s" 'dired-do-shell-command
 )

(defun my-dired-unbind-spc ()
  "Unbind SPC in dired-mode's normal state map."
  (general-unbind :keymaps  'dired-mode-map :states 'normal "SPC"))
(add-hook 'dired-mode-hook #'my-dired-unbind-spc)

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
  :config
  (evil-collection-init))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(put 'dired-find-alternate-file 'disabled nil)

(use-package pulsar
  :defer t
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))
