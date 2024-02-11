
;; package management

;; bootstrap straight


(setq straight-check-for-modifications nil)
(setq straight-check-for-modifications 'live)


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

;; packages

(straight-use-package 'lsp-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'hl-line)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'company)
(straight-use-package 'helm-xref)
(straight-use-package 'dap-mode)
(straight-use-package 'json-mode)
(straight-use-package 'move-text)
(straight-use-package 'wrap-region)
(straight-use-package 'lsp-ui)
(straight-use-package 'sideline)
(straight-use-package 'sideline-lsp)
(straight-use-package 'clang-format)
(straight-use-package 'emmet-mode)
(straight-use-package 'resize-window)
(straight-use-package 'magit)
(straight-use-package 'prettier-js)
(straight-use-package 'typescript-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'go-mode)
(straight-use-package 'php-mode)
(straight-use-package 'lsp-pyright)
(straight-use-package 'py-autopep8)
(straight-use-package 'wakatime-mode)
(straight-use-package 'csharp-mode)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'quelpa)
(straight-use-package 'quelpa-use-package)
(straight-use-package 'ido-completing-read+)
(straight-use-package 'smex)
(straight-use-package 'projectile)
(straight-use-package 'ag)
(straight-use-package 'multiple-cursors)
(straight-use-package 'neotree)
(straight-use-package 'all-the-icons)
(straight-use-package 'lsp-pascal)
(straight-use-package 'yaml-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'company-lua)
(straight-use-package 'company-web)
(straight-use-package 'python-mode)
(straight-use-package 'company-math)
(straight-use-package 'elixir-mode)
(straight-use-package 'seq)
(straight-use-package 'transpose-frame)
(straight-use-package 'slint-mode)
(straight-use-package 'zenburn-theme)
(straight-use-package 'dune-format)
(straight-use-package 'expand-region)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
 ;; for debug

;; (straight-use-package 'benchmark-init)

;; (benchmark-init/activate)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (message "Initialization completed in %s" (emacs-init-time))
;;             (benchmark-init/show-durations-tabulated)))

;; (add-hook 'after-init-hook 'benchmark-init/deactivate)


;; binds

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-'") 'kill-line)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-e") 'move-end-of-line)
(global-set-key (kbd "C-M-SPC") 'mark-word)
(global-set-key (kbd "C-{") 'er/contract-region)
(global-set-key (kbd "C-}") 'er/expand-region)
(global-set-key (kbd "C-S-f") 'query-replace)


;; tabs

(setq-default tab-width 3)

;; select current line

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(global-set-key (kbd "C-S-l") 'select-current-line)


;; duplicate-line


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





;; isearch-mode



(define-key isearch-mode-map (kbd "C-n") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-p") 'isearch-repeat-backward)

;; move between panes with S-<arrows>


(windmove-default-keybindings)


;; resize panes

(global-set-key (kbd "C-c ;") 'resize-window)



;; Delete word backwards (like in terminal)

(defun delete-backward-word (&optional arg)
  "`delete-backward-char`, but if region is active then kill region.
With prefix arg N, delete backward to the start of the Nth word."
  (interactive "P")
  (cond (arg
         (backward-delete-word (prefix-numeric-value arg)))
        ((use-region-p)
         (kill-region (region-beginning) (region-end)))
		  (t (backward-delete-word (prefix-numeric-value arg)))))

(defun backward-delete-word (arg)
  "Like `backward-kill-word`, but just delete."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-w") 'delete-backward-word)




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

;; (set-face-attribute 'default nil
;;                     :family "Fira Code"
;;                     :height 180)



(set-frame-font "Iosevka-21" nil t)


;; start in fullscreen

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; theme

(load-theme 'tsoding t)

(set-face-background 'show-paren-match "#504945")

;; line numbers


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



;; disable sound (annoying)

(setq visible-bell t)
(setq ring-bell-function 'ignore)





;; lsp


(require 'lsp-ui)

(setq lsp-ui-flycheck-live-reporting nil)


(setq lsp-ui-sideline-enable t
      lsp-ui-sideline-show-symbol nil
      lsp-ui-sideline-show-hover nil
      lsp-ui-sideline-show-code-actions nil
      lsp-ui-sideline-show-diagnostics t)

(require 'sideline)
(setq sideline-backends-right '(sideline-lsp))

(require 'lsp)
(add-hook 'lsp-mode-hook 'sideline-mode)


;; flycheck

(require 'flycheck)
(setq flycheck-check-syntax-automatically '(save)) ; Check syntax only on save


;; company

(require 'company)
(add-to-list 'company-backends 'company-dabbrev)

(global-company-mode)


;; yasnippet
(require 'yasnippet)
(yas-global-mode)

;; C/C++

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; format on save
(setq clang-format-style "file")
(setq clang-format-fallback-style "llvm")
(setq clang-format-executable "/usr/bin/clang-format")


(defun clang-format-on-save ()
  (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'clang-format-on-save)

;; end C/C++ =========================


;; Web

;; HTML with LSP, Emmet and Prettier
(add-hook 'html-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            ))

;; CSS with LSP, Emmet and Prettier
(add-hook 'css-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            ))

;; JavaScript with LSP, Emmet and Prettier
(add-hook 'js-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
				))

;; TypeScript with LSP, Emmet and Prettier
(add-hook 'typescript-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            ))


;; TSX with LSP, Emmet and Prettier
;; Install web-mode first
(add-hook 'web-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            ))

;; SCSS with LSP, Emmet and Prettier
;; Install scss-mode first
(add-hook 'scss-mode-hook
          (lambda ()
            (lsp)
            (emmet-mode)
            ))


(eval-after-load "emmet-mode"
  '(progn
     (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)
     (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)))

;; PHP

(add-hook 'php-mode-hook
          (lambda ()
            (lsp)))


(defun lsp-format-buffer-on-save ()
  (when (eq 'php-mode major-mode)
    (lsp-format-buffer)))

;; PHP end ===========

(add-hook 'before-save-hook #'lsp-format-buffer-on-save)


(defun my-prettier-js-before-save ()
  (when (and (or (eq major-mode 'scss-mode)
                 (eq major-mode 'css-mode)
                 (eq major-mode 'javascript-mode)
                 (eq major-mode 'typescript-mode)

					  )
             (executable-find "prettier"))
    (prettier-js)))

(add-hook 'before-save-hook #'my-prettier-js-before-save)

;; end Web ======================================



;; Rust


(add-hook 'rust-mode-hook 'lsp)


(setq rust-format-on-save t)



;; end Rust ================



;; Lua

(add-hook 'lua-mode-hook 'lsp)

;; Lua end ================

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

(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py\\'" . python-mode) auto-mode-alist))


(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))


(use-package py-autopep8
  :straight t
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

(add-hook 'before-save-hook 'lsp-format-buffer)


(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-interactive-switch)
            (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-process-load-file)))





;; Haskell end =============


(setq quelpa-update-melpa-p nil)


;; Odin




(straight-use-package
 '(odin-mode :type git :host github :repo "mattt-b/odin-mode"))

(setq-default lsp-auto-guess-root t) ;; if you work with Projectile/project.el this will help find the ols.json file.
(defvar lsp-language-id-configuration '((odin-mode . "odin")))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/home/cofeek/Desktop/codes/tools/ols/ols")
                  :major-modes '(odin-mode)
                  :server-id 'ols
                  :multi-root t)) ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
(add-hook 'odin-mode-hook #'lsp)

;; Odin end =============

;; Dart

;; Dart end =============



;; FASM

(add-to-list 'load-path "~/.emacs.d/packages/fasm-mode/")
(require 'fasm-mode)

;; FASM end =============


;; NASM


;; NASM end =============



;; Prisma



(straight-use-package
 '(prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode"))

(require 'prisma-mode)


(add-to-list 'lsp-language-id-configuration '(prisma-mode . "prisma"))

;; npm i -g @prisma/language-server

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "prisma-language-server")
                  :major-modes '(prisma-mode)
                  :server-id 'prisma-ls))



(add-hook 'prisma-mode-hook #'lsp)



(add-hook 'prisma-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))

;; Prisma end ==============


;; Elixir



(add-hook 'elixir-mode-hook 'lsp)
(add-hook 'before-save-hook 'lsp-format-buffer)


;; Elixir end ==============

;; Pascal

(add-to-list 'auto-mode-alist '("\\.pas\\'" . opascal-mode))


(require 'lsp-pascal)
(add-hook 'opascal-mode-hook 'lsp)

(add-hook 'opascal-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; Pascal end ==============


;; Ocaml

(use-package tuareg
  :straight t
  )


(use-package merlin
  :straight t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  )

(use-package merlin-eldoc
  :straight t
  :hook ((tuareg-mode) . merlin-eldoc-setup))


(use-package ocamlformat
  :straight t
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))

(add-to-list 'load-path "~/.emacs.d/packages/dune/")
(require 'dune)

(add-hook 'dune-mode-hook 'dune-format-on-save-mode)

;; Ocaml end ================


;; YAML



;; YAML end ==============


;; Slint (UI framework)

(add-hook 'slint-mode-hook 'lsp)

;; Slint (UI framework) end ==============


;; Clojure

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))


(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)

;; Clojure end =================


;; lsp binds

(global-set-key (kbd "C-p") 'company-complete)

;; C-j to go to next error/warn/info

(add-hook 'python-mode-hook
          (lambda()
				(local-unset-key (kbd "C-j"))))


(global-set-key (kbd "C-j") 'flycheck-next-error)


;; C-c . to execute code action

(global-set-key (kbd "C-c .") 'lsp-execute-code-action)


;; f2 to rename

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename))


;; end lsp binds ======================




;; end lsp ===================================


;; moving lines


(use-package move-text
  :straight t
  :bind
  (("M-<up>"   . move-text-up)
   ("M-<down>" . move-text-down)))


;; brackets autopair and wrapping

(electric-pair-mode 1)


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



(define-key dired-mode-map (kbd "C-<return>") 'dired-find-file-other-window)



;; git (magit)


;; time control (wakatime)

(global-wakatime-mode)





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

(global-set-key (kbd "C-\\") 'shell-command)
(global-set-key (kbd "M-\\") 'async-shell-command)

(setq shell-file-name "/bin/bash")



;; Multiple Cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c")      'mc/edit-lines)
(global-set-key (kbd "C-c C->")          'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<")          'mc/mark-previous-like-this)
(global-set-key (kbd "C-\"")             'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")              'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-<down-mouse-1>") 'mc/add-cursor-on-click)




;; NeoTree

(require 'neotree)
(global-set-key (kbd "C-b") 'neotree-toggle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Latex for notes

(use-package auctex
  :defer    t
  :straight t
  )



(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-prettify-symbols-mode)
(add-hook 'LaTeX-math-mode-hook 'turn-on-prettify-symbols-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
(add-hook 'LaTeX-math-mode-hook 'turn-on-flyspell)



(setq TeX-electric-sub-and-superscript t)

(require 'company)
(require 'company-math)

(add-to-list 'company-backends 'company-math-symbols-unicode)
(add-to-list 'company-backends 'company-math-symbols-latex)


(defun latex-binds ()
  (define-key LaTeX-mode-map (kbd "C-c C-x C-l") 'org-latex-preview))

(add-hook 'LaTeX-mode-hook 'latex-binds)

(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))


;; Latex end ==============


;; UML




(add-to-list 'load-path "~/.emacs.d/packages/plantuml-mode/")
(require 'plantuml-mode)


(setq plantuml-jar-path "/usr/bin/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))



(add-to-list 'load-path "~/.emacs.d/packages/flycheck-plantuml/")

(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))



;; UML end ========

;; Graphviz dot

(add-to-list 'load-path "~/.emacs.d/packages/graphviz-dot-mode/")

;; Graphviz dot end  ========


;; D2lang


(add-to-list 'load-path "~/.emacs.d/packages/d2-mode/")
(require 'd2-mode)

(setq d2-output-format ".png")


;; D2lang end ================

;; Org

(require 'org)

(setq org-agenda-files '("~/Desktop/notes/"))

(setq org-log-done t)

(add-to-list 'load-path "~/.emacs.d/packages/org-table-wrap/")
(require 'org-table-wrap-functions)

(define-key org-mode-map (kbd "C-|") 'org-table-column-wrap-to-point)

(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
		'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o -jobname=%b %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o -jobname=%b %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o -jobname=%b %f"
        "rm -rf _minted-*"))

(setq org-src-fontify-natively t)


;; Org end ========

;; Markdown

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/emacs-livedown"))

;; Markdown end ========

;; Editorconfig

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; Editorconfig end ========


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	'("0d747b714779e0148e602581746b8806926724a72fb247129c7359f5f6ad80c2" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" default))
 '(doc-view-continuous t)
 '(org-support-shift-select 'always)
 '(plantuml-indent-level 3)
 '(plantuml-suppress-deprecation-warning nil)
 '(wakatime-cli-path "~/.wakatime/wakatime-cli")
 '(warning-suppress-types '(((org-element org-element-parser)) (comp))))






(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
 '(Man-underline ((t (:foreground "tomato" :underline t))))
 '(button ((t (:inherit link :weight normal))))
 '(css-selector ((t (:inherit font-lock-type-face))))
 '(font-latex-script-char-face ((t (:foreground "Green"))))
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#949bb0" :weight bold))))
 '(font-latex-warning-face ((t (:inherit bold :foreground "tomato"))))
 '(ido-first-match ((t (:foreground "#FFDD33" :weight bold))))
 '(ido-incomplete-regexp ((t (:foreground "white"))))
 '(ido-only-match ((t (:foreground "#FF9B21"))))
 '(ido-subdir ((t (:foreground "#949bb0" :weight bold))))
 '(italic ((t (:slant italic :weight normal))))
 '(makefile-targets ((t (:inherit font-lock-type-face))))
 '(mmm-default-submode-face ((t nil)))
 '(org-document-info-keyword ((t (:inherit font-lock-type-face))))
 '(org-level-1 ((t (:extend nil :weight bold))))
 '(org-level-2 ((t (:inherit font-lock-type-face :extend nil :weight bold))))
 '(preview-face ((t nil)))
 '(rust-unsafe ((t (:inherit font-lock-keyword-face))))
 '(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face))))
 '(tuareg-font-lock-operator-face ((t (:inherit font-lock-keyword-face)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
