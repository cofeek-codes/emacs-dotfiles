(setq package-enable-at-startup nil)

(setq inhibit-startup-screen t)

(setq gc-cons-threshold (*  50  1000  1000))
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (*  2  1000  1000))))
