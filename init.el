;;binds

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-c g") 'goto-line)

;; isearch-mode



(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)


;; move between panes with S-<arrows>

(windmove-default-keybindings)


;; resize panes

(global-set-key (kbd "C-c ;") 'resize-window)

;; vscode-like comments

(defun toggle-comment-line-or-region ()
  "Toggle comment for lines in a region or the current line"
  (interactive)
  (if (region-active-p)
      (let ((region-start (min (region-beginning) (region-end)))
            (region-end (max (region-beginning) (region-end))))
        (save-excursion
          (goto-char region-start)
          (beginning-of-line)
          (setq region-start (point))
          (goto-char region-end)
          (end-of-line)
          (setq region-end (point)))
        (comment-or-uncomment-region region-start region-end))
    (comment-line 1)))

(global-set-key (kbd "C-/") 'toggle-comment-line-or-region)

;; customization

;; font

(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 180)

;; start in fullscreen

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; theme

(load-theme 'tsoding t)

(set-face-background 'show-paren-match "#504945")

;; relative numbers


     (require 'hl-line)

(defface my-linum-hl
  `((t :inherit linum :background nil :foreground "#ffdd33"))
  "Face for the current line number."
  :group 'linum)


     (defadvice linum-update (around my-linum-update)
       (let ((my-linum-current-line-number (line-number-at-pos)))
         ad-do-it))
     (ad-activate 'linum-update)

     (setq linum-format 'my-linum-format)

     (defun my-linum-format (line-number)
       (propertize (format "%3d" line-number)
                   'face (if (eq line-number my-linum-current-line-number)
                             'my-linum-hl
                           'linum)))

     (add-hook 'prog-mode-hook 'linum-mode)



;; disable sound (annoyng)

     (setq visible-bell t)
     (setq ring-bell-function 'ignore)





;; package management

(require 'package)
(unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
 ;; (package-refresh-contents) don't want it to refresh every time


(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp hl-line  projectile hydra flycheck company avy which-key helm-xref dap-mode json-mode move-text lsp-ui sideline sideline-lsp clang-format emmet-mode resize-window magit prettier-js typescript-mode rust-mode go-mode php-mode telega lsp-pyright py-autopep8 wakatime-mode))
 

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


;; lsp


(with-eval-after-load 'lsp-mode
  ;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (setq lsp-keymap-prefix "C-c l")
  (require 'dap-cpptools)
  (yas-global-mode))







(use-package lsp-ui
  :commands lsp-ui-mode)

   (setq lsp-ui-sideline-enable t
         lsp-ui-sideline-show-symbol nil
         lsp-ui-sideline-show-hover nil
         ;; lsp-ui-sideline-show-flycheck t
         lsp-ui-sideline-show-code-actions nil
         lsp-ui-sideline-show-diagnostics t)

(use-package sideline
  :init
  (setq sideline-backends-right '(sideline-lsp)))
   
(use-package lsp-mode :hook (lsp-mode . sideline-mode))  ; enable it when lsp is on
;; (use-package lsp-ui :init (setq lsp-ui-sideline-enable nil))  ; disable original sideline


;; C/C++

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)




;; format on save
(setq clang-format-style "file")
(setq clang-format-fallback-style "llvm")
(setq clang-format-executable "/usr/bin/clang-format")


(defun clang-format-on-save ()
  (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'clang-format-on-save)

;; end C/C++=========================


;; Web

;; HTML with LSP, Emmet and Prettier
(add-hook 'html-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            (prettier-js-mode)))

;; CSS with LSP, Emmet and Prettier
(add-hook 'css-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            (prettier-js-mode)))

;; JavaScript with LSP, Emmet and Prettier
(add-hook 'js-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            (prettier-js-mode)))

;; TypeScript with LSP, Emmet and Prettier
(add-hook 'typescript-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            (prettier-js-mode)))


;; TSX with LSP, Emmet and Prettier
;; Install web-mode first
(add-hook 'web-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            (prettier-js-mode)))

;; SCSS with LSP, Emmet and Prettier
;; Install scss-mode first
(add-hook 'scss-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            (prettier-js-mode)))



(setq prettier-js-args '("--no-semi"
                         "--single-quote"
                         "--jsx-single-quote"
                         "--arrow-parens" "avoid"))



(eval-after-load "emmet-mode"
  '(progn
     (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)
     (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)))



(add-hook 'php-mode-hook
          (lambda ()
            (lsp)))


(defun lsp-format-buffer-on-save ()
  (when (eq 'php-mode major-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook #'lsp-format-buffer-on-save)

;; end Web ======================================



;; Rust


(add-hook 'rust-mode-hook 'lsp)


(setq rust-format-on-save t)



;; end Rust ================


;; Go


(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Go end =============

;; Python


;; (add-hook 'python-mode-hook 'lsp)


(use-package lsp-pyright
       :ensure t
       :hook (python-mode . (lambda ()
                               (require 'lsp-pyright)
                               (lsp))))


     (use-package py-autopep8
       :hook (python-mode . py-autopep8-mode))

;; Python end ==========


;; lsp binds

;; C-j to go to next error/warn/info


(defun my-next-error ()
  (interactive)
  (condition-case nil
      (next-error)
    (user-error
     (goto-char (point-min))
     (next-error))))




(eval-after-load "emmet-mode"
  '(progn
     (define-key emmet-mode-keymap (kbd "C-j") nil)))

(global-set-key (kbd "C-j") 'my-next-error)


;; C-c . to execute code action

(global-set-key (kbd "C-c .") 'lsp-execute-code-action)


;; f2 to rename

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename))

;; end lsp binds ======================




;; end lsp ===================================


;; moving lines

(use-package move-text
  :bind
  (("M-<up>"   . move-text-up)
   ("M-<down>" . move-text-down)))


;; brackets autopair and wrapping

(electric-pair-mode 1)

(unless (package-installed-p 'wrap-region)
  (package-refresh-contents)
  (package-install 'wrap-region))

(require 'wrap-region)
(wrap-region-mode t)


;; set directory color in dired mode

(add-hook 'dired-mode-hook
          (lambda ()
            (set-face-foreground 'dired-directory "#949bb0")))



;; git (magit)


;; time control (wakatime)

(global-wakatime-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(py-autopep8 typescript-mode emmet-mode lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode json-mode move-text lsp-ui sideline sideline-lsp clang-format))
  '(wakatime-cli-path "~/.wakatime/wakatime-cli"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
