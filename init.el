;; keybinds

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'query-replace)
(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-'") 'kill-line)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-p") 'company-complete)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-c ;") 'resize-window)
(global-set-key (kbd "C-c C-c") 'compile)

(windmove-default-keybindings)

(defun mark-whole-line ()
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position))
  )

(global-set-key (kbd "C-S-l") 'mark-whole-line)

(defun duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C--") 'duplicate-line)

;; isearch keybinds

(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "C-r") 'isearch-toggle-regexp)

;; compile-mode keybinds

(defun setup-compilation-keybinds ()
  (local-set-key (kbd "r") 'recompile)
  (local-set-key (kbd "C-c C-c") 'kill-compilation))

(add-hook 'compilation-mode-hook 'setup-compilation-keybinds)

;; line numbers

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; lsp

(defun setup-lsp-mode ()
(flymake-mode 0))

(add-hook 'lsp-mode-hook 'setup-lsp-mode)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output t)
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes t)
 '(default-frame-alist '((font . "Iosevka-16") (fullscreen . maximized)))
 '(default-input-method "russian-computer")
 '(delete-selection-mode t)
 '(electric-pair-mode t)
 '(global-company-mode t)
 '(ido-everywhere t)
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lsp-auto-guess-root t)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-enable-suggest-server-download nil)
 '(lsp-format-buffer-on-save t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(magit-no-confirm '(stage-all-changes unstage-all-changes set-and-push))
 '(make-backup-files nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(graphviz-dot-mode d2-mode plantuml-mode lsp-mode resize-window company magit gruber-darker-theme))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#181818" :foreground "#181818"))))
 '(vertical-border ((t (:foreground "#282828")))))
