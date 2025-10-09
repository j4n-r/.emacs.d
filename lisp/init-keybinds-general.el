;;; init-keybinds-general.el --- Keybindings using General.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Theme

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-auto-unbind-keys)
  (general-create-definer leader
    :prefix "SPC")
  (leader
    :states 'normal
    :keymaps 'override
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
    "cc" '(project-compile :which-key "compile")
    "cf" '(eglot-format-buffer :which-key "format buffer")
    "cF" '(apheleia-format-buffer :which-key "format buffer")
    "cr" '(eglot-rename :which-key "rename")
    "co" '(eglot-code-action-organize-imports :which-key "organize imports")

    "e" '(:ignore t :which-key "eval elisp")
    "eb" '(eval-buffer :which-key "eval buffer")
    "er" '(eval-region :which-key "eval region")
    "ee" '(eval-expression :which-key "eval expression")
    "ed" '(eval-defun :which-key "eval at point")

    "f" '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "fr" '(rename-visited-file :which-key "rename file")
    "fa" '(ff-find-other-file :which-key "find alternate file") ;; e.g. find header

    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit")
    "gb" '(magit-blame  :which-key "magit blame")
    "gd" '(magit-diff-buffer-file  :which-key "magit diff")
    "gr" '(magit-refresh-buffer  :which-key "magit refresh buffer")

    "h" '(:ignore t :which-key "help")
    "hw" '(woman :which-key "woman")

    "o" '(:igore t :which-key "open/org")
    "od" '(dired-jump :which-key "open dired")
    "ot" '(vterm-other-window :which-key "open project terminal")
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
    "pc" '(project-compile :which-key "project compile")
    "pt" '(consult-todo-project :which-key "todos in project")

    "qr" '(query-replace :which-key "Query Replace")
    "qR" '(project-query-replace-regexp :which-key "Query regex Replace")
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
    "w=" '(balance-windows-area :which-key "balance windows")
    "wr" '(evil-window-rotate-downwards :which-key "rotate downwards")
    "wR" '(evil-window-rotate-upwards :which-key "rotate upwards")
    "wt" '(window-toggle-side-windows :which-key "toggle side windows")
    )
  (general-define-key
   :states 'normal
   :keymaps 'override
   "M-a" 'project-async-shell-command
   "M-o" 'ace-window
   "M-A" 'async-shell-command
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
   "K"   'eldoc-box-help-at-point
   "s-k" 'display-local-help
   "]e" 'next-error
   "[e" 'previous-error
   "gcc" '(lambda ()
            (interactive)
            (if (not (use-region-p))
                (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
   )
  
  (general-define-key
   :states 'insert
    "M-t" 'tab-to-tab-stop)

  (general-define-key
   :states 'motion
   "gc" 'comment-or-uncomment-region
   "gC" 'comment-box)

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
   ))

(provide 'init-keybinds-general)
;;; init-keybinds-general.el ends here.
