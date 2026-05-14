;;; init-dired.el --- Dired -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; This configuration customizes `dired' to enhance its usability. The settings
;; below specify how file listings are displayed, the target for file operations,
;; and associations for opening various file types with their respective applications.
;; For example, image files will open with `feh', while audio and video files
;; will utilize `mpv'.
(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  ;; :bind (:map dired-mode-map
  ;;             ("f" . find-file)
  ;;             ("n" . dired-create-empty-file)
  ;;             ("N" . dired-create-directory)
  ;;             ("h" . dired-up-directory)
  ;;             ("l" . dired-find-alternate-file)
  ;;             ("W" . wdired-change-to-wdired-mode))
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

(use-package wdired
  :ensure nil
  :bind (:map wdired-mode-map
              ("A" . wdired-abort-changes)))
(provide 'init-dired)
;;; init-dired.el ends here.

