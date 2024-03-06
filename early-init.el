(setenv "LSP_USE_PLISTS" "true")

(setq package-enable-at-startup nil)

(setq inhibit-startup-screen t)

(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq gc-cons-threshold (*  50  1000  1000))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (*  2  1000  1000))))
