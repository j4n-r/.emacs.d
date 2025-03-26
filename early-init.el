(setenv "LSP_USE_PLISTS" "true") ;; in early-init.el
(setq package-enable-at-startup nil)

(setq default-frame-alist '((fullscreen . maximized)
                            ;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)

                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#191724")
                            (foreground-color . "#c4a7e7")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))
