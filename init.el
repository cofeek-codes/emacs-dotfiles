;; binds

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-e") 'move-end-of-line)


;; duplicate-line

(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (forward-line)
    (insert current-line)
    (forward-line -1)))

(global-set-key (kbd "C-c C-<down>") 'duplicate-line)


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


(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp hl-line  projectile hydra flycheck company avy which-key helm-xref dap-mode json-mode move-text lsp-ui sideline sideline-lsp clang-format emmet-mode resize-window magit prettier-js typescript-mode rust-mode go-mode php-mode telega lsp-pyright py-autopep8 wakatime-mode company-tabnine csharp-mode haskell-mode lsp-haskell cargo quelpa quelpa-use-package ido-completing-read+ smex projectile ag multiple-cursors neotree all-the-icons))
 

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


;; C#

(add-hook 'csharp-mode-hook 'lsp)

(defun csharp-mode-before-save-hook ()
  (when (eq major-mode 'csharp-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'csharp-mode-before-save-hook)


;; C# end ===


;; Haskell

(add-hook 'haskell-mode-hook 'lsp)
(add-hook 'haskell-literate-mode-hook 'lsp)

(setq lsp-haskell-server-path "/home/cofeek/.ghcup/bin/haskell-language-server-wrapper")

(add-hook 'before-save-hook 'lsp-format-buffer)

;; Haskell end =============


;; Odin

     (quelpa
      '(quelpa-use-package
        :fetcher git
        :url "https://github.com/quelpa/quelpa-use-package.git"))
     (require 'quelpa-use-package)

    (use-package odin-mode
      :quelpa (odin-mode :repo "mattt-b/odin-mode" :fetcher github))

(setq-default lsp-auto-guess-root t) ;; if you work with Projectile/project.el this will help find the ols.json file.
(defvar lsp-language-id-configuration '((odin-mode . "odin")))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/home/cofeek/Desktop/codes/tools/ols/ols")
                  :major-modes '(odin-mode)
                  :server-id 'ols
                  :multi-root t)) ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
(add-hook 'odin-mode-hook #'lsp)

;; Odin end =============


;; Prisma

(add-to-list 'load-path "~/.emacs.d/emacs-prisma-mode")
(require 'prisma-mode)

(add-to-list 'lsp-language-id-configuration '(prisma-mode . "prisma"))
(add-hook 'prisma-mode-hook #'lsp-deferred)


(add-hook 'prisma-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))

;; Prisma end ==============

;; lsp binds

(global-set-key (kbd "C-p") 'company-complete)

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



;; C-c d to go to definition

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c d") #'lsp-find-definition))


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


;;; dired
(require 'dired-x)

(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")


;; set directory color in dired mode

(add-hook 'dired-mode-hook
          (lambda ()
            (set-face-foreground 'dired-directory "#949bb0")))




;; git (magit)


;; time control (wakatime)

(global-wakatime-mode)





;; tabnine

;; (require 'company-tabnine)

;; (add-to-list 'company-backends #'company-tabnine)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)



;; no backup files

(setq make-backup-files nil)




;; emmet-mode

(eval-after-load 'emmet-mode
  '(progn
    (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)))



;; minibuffer


(require 'ido-completing-read+)
(require 'smex)


(setq ido-enable-flex-matching t)
(ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)


(setq ido-use-filename-at-point 'guess)

(setq ido-create-new-buffer 'always)





;; projectile

(projectile-mode +1)

(define-key projectile-mode-map (kbd "C-d") 'projectile-find-file)

;; Compilation


(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq compilation-scroll-output t)

(defun my-compilation-mode-hook ()
  "Custom compilation mode hook."
  (local-set-key (kbd "r") 'recompile)
  (local-set-key (kbd "C-c C-c") 'kill-compilation))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)


;; Shell

(global-set-key (kbd "C-c !") 'shell-command)

(setq shell-file-name "/bin/bash")



;; Multiple Cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)
;; (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)


;; NeoTree

(require 'neotree)
(global-set-key (kbd "C-b") 'neotree-toggle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Markdown

(custom-set-variables
 '(markdown-command "/usr/bin/pandoc"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-preview-mode multiple-cursors wrap-region lsp-mode yasnippet lsp-treemacs helm-lsp hl-line projectile hydra flycheck company avy which-key helm-xref dap-mode json-mode move-text lsp-ui sideline sideline-lsp clang-format emmet-mode resize-window magit prettier-js typescript-mode rust-mode go-mode php-mode telega lsp-pyright py-autopep8 wakatime-mode company-tabnine csharp-mode haskell-mode lsp-haskell cargo quelpa quelpa-use-package ido-completing-read+ smex projectile ag))
 '(wakatime-cli-path "~/.wakatime/wakatime-cli"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-first-match ((t (:foreground "#FFDD33" :weight bold))))
 '(ido-incomplete-regexp ((t (:foreground "white"))))
 '(ido-only-match ((t (:foreground "#FF9B21"))))
 '(ido-subdir ((t (:foreground "#949bb0" :weight bold)))))
