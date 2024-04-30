;; general pre-init

(setq redisplay-dont-pause t)


(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 3
              indent-tabs-mode nil
              compilation-scroll-output t
              default-input-method "russian-computer"
				  )


;; package management

;; bootstrap straight

(setq straight-check-for-modifications nil)

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

(straight-use-package 'use-package)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'yasnippet)
(straight-use-package 'hl-line)
(straight-use-package 'highlight-numbers)
(straight-use-package 'projectile)
(straight-use-package 'flycheck)
(straight-use-package 'company)
(straight-use-package 'dap-mode)
(straight-use-package 'json-mode)
(straight-use-package 'web-mode)
(straight-use-package 'dotenv-mode)
(straight-use-package 'move-text)
(straight-use-package 'wrap-region)
(straight-use-package 'clang-format)
(straight-use-package 'emmet-mode)
(straight-use-package 'resize-window)
(straight-use-package 'magit)
(straight-use-package 'typescript-mode)
(straight-use-package 'go-mode)
(straight-use-package 'php-mode)
(straight-use-package 'lsp-pyright)
(straight-use-package 'py-autopep8)
(straight-use-package 'wakatime-mode)
(straight-use-package 'csharp-mode)
(straight-use-package 'haskell-mode)
(straight-use-package 'lsp-haskell)
(straight-use-package 'ido-completing-read+)
(straight-use-package 'smex)
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
(straight-use-package 'edit-indirect)
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
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word-or-char)

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


(delete-selection-mode 1)


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



(set-frame-font "Iosevka-18" nil t)


;; start in fullscreen

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; theme

(load-theme 'jblow t)

(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; line numbers


(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)))

(defun post-text-scale-callback ()
 ;; fix line number text size
 (set-face-attribute 'line-number nil
                      :height (floor (* (face-attribute 'default :height)
                                        (expt text-scale-mode-step text-scale-mode-amount)))))
(add-hook 'text-scale-mode-hook 'post-text-scale-callback)

(setq display-line-numbers-current-absolute nil)


;; disable sound (annoying)

(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; lsp

(setq-default lsp-auto-guess-root t)

(setq lsp-diagnostics-provider :none)

(setq lsp-lens-enable nil)

;; flycheck

;; C/C++

(defun enable-lsp-for-c/cpp-modes ()
  "Enable lsp-mode for C/C++ files."
  (lsp))

(add-hook 'c-mode-hook #'enable-lsp-for-c/cpp-modes)
(add-hook 'c++-mode-hook #'enable-lsp-for-c/cpp-modes)

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

;; auto-rename-tag

(defun setup-art ()
 "Enable auto-rename-tag-mode for XML, HTML, PHP, JSX, and TSX modes."
 (add-hook 'nxml-mode-hook 'auto-rename-tag-mode)
 (add-hook 'html-mode-hook 'auto-rename-tag-mode)
 (add-hook 'php-mode-hook 'auto-rename-tag-mode)
 (add-hook 'js-jsx-mode-hook 'auto-rename-tag-mode)
 (add-hook 'typescript-mode-hook 'auto-rename-tag-mode))

(use-package auto-rename-tag
  :ensure   t
  :straight t
 :config
 (setup-art))


(defun setup-webmode ()
  "Config for web modes"
  (lsp)
  (emmet-mode)
  )

;; HTML with LSP, Emmet and Prettier
(add-hook 'html-mode-hook #'setup-webmode)
;; CSS with LSP, Emmet and Prettier
(add-hook 'css-mode-hook #'setup-webmode)
;; JavaScript with LSP, Emmet and Prettier
(add-hook 'js-mode-hook #'setup-webmode)
;; TypeScript with LSP, Emmet and Prettier
(add-hook 'typescript-mode-hook #'setup-webmode)
;; SCSS with LSP, Emmet and Prettier
(add-hook 'scss-mode-hook #'setup-webmode)
;; Install scss-mode first

(add-hook 'web-mode-hook 'emmet-mode)

(eval-after-load "emmet-mode"
  '(progn
     (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)
     (define-key emmet-mode-keymap (kbd "C-,") 'emmet-expand-line)))

;; PHP
(with-eval-after-load 'php-mode
(add-hook 'php-mode-hook
          (lambda ()
            (lsp)))

(defun lsp-format-buffer-on-save ()
  (when (eq 'php-mode major-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook #'lsp-format-buffer-on-save)
)

;; PHP end ===========

;; end Web ======================================

;; Rust

(use-package rust-mode
  :straight t
  )

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'lsp)
  )


(setq rust-format-on-save t)

;; Rust Pest

(use-package pest-mode
  :straight (:host github :repo "ksqsf/pest-mode")
  :mode "\\.pest\\'"
  )

;; end Rust ================

;; Lua

(with-eval-after-load 'lua-mode
(add-hook 'lua-mode-hook 'lsp)
)
;; Lua end ================

;; Go

(with-eval-after-load 'go-mode
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

)

;; Go end =============

;; Python
(with-eval-after-load 'python
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

)
;; Python end ==========

;; C#

(with-eval-after-load 'csharp-mode
(add-hook 'csharp-mode-hook 'lsp)
(defun csharp-mode-before-save-hook ()
  (when (eq major-mode 'csharp-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'csharp-mode-before-save-hook)
)

;; C# end ===

;; Haskell

(with-eval-after-load 'haskell-mode
(add-hook 'haskell-mode-hook 'lsp)
(add-hook 'haskell-literate-mode-hook 'lsp)

(add-hook 'before-save-hook 'lsp-format-buffer)


(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-interactive-switch)
            (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-process-load-file)))

)


;; Haskell end =============


;; Odin




;; (straight-use-package
;;  '(odin-mode :type git :host github :repo "mattt-b/odin-mode"))

;; (setq-default lsp-auto-guess-root t) ;; if you work with Projectile/project.el this will help find the ols.json file.
;; (defvar lsp-language-id-configuration '((odin-mode . "odin")))
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection "/home/cofeek/Desktop/codes/tools/ols/ols")
;;                   :major-modes '(odin-mode)
;;                   :server-id 'ols
;;                   :multi-root t)) ;; This is just so lsp-mode sends the "workspaceFolders" param to the server.
;; (add-hook 'odin-mode-hook #'lsp)

;; Odin end =============

;; Dart

;; Dart end =============



;; FASM


;; FASM end =============


;; NASM


;; NASM end =============



;; Prisma



;; (straight-use-package
;;  '(prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode"))

;; (require 'prisma-mode)


;; (add-to-list 'lsp-language-id-configuration '(prisma-mode . "prisma"))

;; ;; npm i -g @prisma/language-server

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection "prisma-language-server")
;;                   :major-modes '(prisma-mode)
;;                   :server-id 'prisma-ls))



;; (add-hook 'prisma-mode-hook #'lsp)



;; (add-hook 'prisma-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))

;; Prisma end ==============


;; Elixir


(with-eval-after-load 'elixir-mode
(add-hook 'elixir-mode-hook 'lsp)
(add-hook 'before-save-hook 'lsp-format-buffer)
)

;; Elixir end ==============

;; Pascal

(add-to-list 'auto-mode-alist '("\\.pas\\'" . opascal-mode))

(with-eval-after-load 'opascal-mode
(require 'lsp-pascal)
(add-hook 'opascal-mode-hook 'lsp)

(add-hook 'opascal-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
)
;; Pascal end ==============


;; Ocaml

(use-package tuareg-mode
  :straight t
  :mode "\\.ml\\'" ; Autoload tuareg when opening .ml files
  :hook ((tuareg-mode . merlin-mode)
         (merlin-mode . company-mode)))

(use-package merlin-eldoc
  :straight t
  :after tuareg
  :hook ((tuareg-mode . merlin-eldoc-setup)))

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
(with-eval-after-load 'slint-mode
(add-hook 'slint-mode-hook 'lsp)
)
;; Slint (UI framework) end ==============


;; Clojure

(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(with-eval-after-load 'clojure-mode
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
)
;; Clojure end =================


;; lsp binds


(with-eval-after-load 'lsp-mode
;; C-j to go to next error/warn/info

(add-hook 'python-mode-hook
          (lambda()
				(local-unset-key (kbd "C-j"))))


(global-set-key (kbd "C-j") 'flycheck-next-error)


;; C-c . to execute code action

(global-set-key (kbd "C-c .") 'lsp-execute-code-action)


;; f2 to rename

(define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)

)
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




(define-key dired-mode-map (kbd "C-<return>") 'dired-find-file-other-window)



;; git (magit)


;; time control (wakatime)

(global-wakatime-mode)

;; yasnippet
(require 'yasnippet)
(yas-global-mode)

;; company

(require 'company)
(add-to-list 'company-backends 'company-dabbrev)

(add-to-list 'load-path "~/.emacs.d/packages/company-paths/")
(require 'company-paths)
(add-to-list 'company-backends 'company-paths)

(global-company-mode)

;; Number the candidates (use M-1, M-2 etc to select completions).

;; (setq company-show-numbers t)
;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay 0.1)

(global-set-key (kbd "C-p") 'company-complete)


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

(global-set-key (kbd "C-\|") 'shell-command)
(global-set-key (kbd "M-\\") 'async-shell-command)

(setq shell-file-name "/bin/bash")



;; Multiple Cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c")      'mc/edit-lines)
(global-set-key (kbd "C-c C->")          'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<")          'mc/mark-previous-like-this)
(global-set-key (kbd "C-:")              'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-<down-mouse-1>") 'mc/add-cursor-on-click)




;; NeoTree

(require 'neotree)
(global-set-key (kbd "C-b") 'neotree-toggle)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Latex for notes

;; FIXME: enable auctex


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


(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;; Graphviz dot end  ========


;; D2lang

(add-to-list 'load-path "~/.emacs.d/packages/d2-mode/")

;; Autoload d2-mode
(autoload 'd2-mode "d2-mode" nil t)

;; Associate .d2 files with d2-mode
(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))

;; Set output format for d2-mode
(setq d2-output-format ".png")

;; D2lang end ================


;; QBE


(add-to-list 'load-path "~/.emacs.d/packages/qbe-mode/")
(autoload 'qbe-mode "qbe-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ssa\\'" . qbe-mode))

;; QBE end ================

;; Org
(with-eval-after-load 'org

  (setq org-agenda-files '("~/Desktop/notes/"))

  (setq org-log-done t)

  (add-to-list 'load-path "~/.emacs.d/packages/org-table-wrap/")
  (require 'org-table-wrap-functions)

  (define-key org-mode-map (kbd "C-|") 'org-table-column-wrap-to-point)

  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
		  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o -jobname=%b %f"
			 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o -jobname=%b %f"
			 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o -jobname=%b %f"
			 "rm -rf _minted-*"))

  (setq org-src-fontify-natively t)

  (require 'org)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  )
;; Org end ========

;; Markdown
(with-eval-after-load 'markdown-mode

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/emacs-livedown"))
)
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
 '(css-indent-offset 3)
 '(custom-safe-themes
   '("6dcdae1f438b04bb0076e16e81142b076f3b4e51e67dda41d1f0ddcbacff70ca" "7cc166f11ca541201aec2614bbb54a3c98a45f5efe4ce4a76d5cbdee47920438" "87b0ae928443a231cc9201a7aa90306b2c9dbd320f21dc4fe32fb4af6b70c7e7" "37befde9e032f0223f76c416506a91c0cc414c643fb958fbd138c22cc34da14b" "eaacd560648a035b3a6a3c3e6c76ffe107003a1926170f730e59ee0f9e103a64" "76c54680572577f5a9bd471c35944f640acadb7eba74b413d71e6bccafd81d88" "9aca7350ef93fd373c915d29fa3064215b33b82366f288328dfcfddbe4b8768e" "edd94f58f36a3951aa050d37f1a8bf2057f2c26f9efb1b60f9d2ba8b021533d4" "c85a1da7f64440b8631295854e6980e7174dd6ac2d536b7ded27a8ec8b53f3f9" "69299d40c1e1d989f5302e72001cc6b6c3c80111d9b03a6a876ab95d5db4e48e" "92f35fb2f509e66c31494b0e9ddd63ddcfb00556f36a33bb8f8a546c97e7c704" "8e32b56e2383871ed92186431b3d4ed2c85c83ab7278f7cc3aa2aea9b0def013" "f9d6d868cd73cc3efa68e4f7634cdf3c2c44bcaf7dd60fca2e03ef686ec1d36d" "49800fd9f32a29e880006c6f60b8dac2124d6e78fe852cfc9722d75820798fab" "5573eabe53d841c4e928d98d07476446b57d808b87df3f8a095200356a997d5a" "332588b0b1e09f893b1ecfbcfe37e3db0b0a5d9a1892dfefcf62192048f78482" "49c5bb3db27bfc0c06afdf0eb399753444cd75002c458d9cf738c9dac6635098" "0daac15c5639973d6d5f32e12f1f88a8f6c130955027f7702fea6d28ce7642c8" "c7f7f7f38f6405ff8cdcd6f5a9ae085d1382b16c34b9d1d59123497ffb426b93" "97b89fde4cd3d6edfdf23a28ff64af27623d2094df32169cd411025480e159c1" "a9d704485d36c4db0913ad154c2340478c64b2ac996d189cb95eb62c04dfc2fb" "70331ce081f4cc9be4587e3cfcebae47de9db6dbd8d5be030210ec493f353dd0" "3df72c5bea063dc02ba21d079b372737568f3ec04a51ed209352c6ef61763111" "5a53c87cfe7f4eb8c68cc236a2dd55021b14ab023df33d5c2dc90c6e10bce3d2" "3423b464720571391247246ab47469117269f2281ea66b995010fc458909ee3d" "3748ad397c2d2ea31a7785c50a0383b783467844e34b3f2a251bbd99d7e32435" "8edf506b7e633635b782240b886f5bf01e62bfa7ae4873fad581e3396644d443" "9c6f87eb817893fff742ceea5de988fd1646d93d3f5e26feff4c48bd69db3660" "02b6ced26f476c3f5a66b0c2dad5b75e63d33c393557e49ff8a4ad52e7afe167" "08def275518a325b04f41871ca8ae84759ebe115b620e97570a0aedd9256bc49" "74926c29273b5292813139d5cc8d5b3e72b07a286b127964c2b38b88f92e410a" "f1c97b1d6235aeb12ce4cd010551841a9ba6eb7a8e14ab9273fd8a054f6d0807" "654501d4953b96c4512d5380d2936c4e41d458be0fa005151c58fa9f7a5b4d62" "0d747b714779e0148e602581746b8806926724a72fb247129c7359f5f6ad80c2" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" default))
 '(doc-view-continuous t)
 '(mc/always-run-for-all t)
 '(org-support-shift-select 'always)
 '(plantuml-indent-level 3)
 '(plantuml-suppress-deprecation-warning nil)
 '(rust-indent-offset 4)
 '(standard-indent 3)
 '(typescript-indent-level 3)
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
 '(font-lock-number-face ((t (:foreground "#7ad0c6"))))
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
