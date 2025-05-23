;; general pre-init

(setq redisplay-dont-pause t)


(setq-default inhibit-splash-screen t
              make-backup-files nil
              indent-tabs-mode nil
              tab-width 4
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
(straight-use-package 'resize-window)
(straight-use-package 'magit)
(straight-use-package 'typescript-mode)
(straight-use-package 'phi-search)
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
(straight-use-package 'glsl-mode)
(straight-use-package 'cider)
(straight-use-package 'fireplace)
(straight-use-package 'emojify)
(straight-use-package 'company-emojify)
(straight-use-package 'pdf-tools)
(straight-use-package 'org)
(straight-use-package 'orgnote)
(straight-use-package 'gruber-darker-theme)
(straight-use-package 'todoist)
(straight-use-package 'twig-mode)

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
(global-set-key (kbd "C-f") 'phi-search)
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
(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-M-<left>") 'backward-sexp)
(global-set-key (kbd "C-M-<right>") 'forward-sexp)


;; tabs


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

;; move between panes with S-<arrows>

(windmove-default-keybindings)

;; resize panes

(global-set-key (kbd "C-c ;") 'resize-window)



;; Delete word backwards (like in terminal)

(global-set-key (kbd "C-w")
                (lambda ()
                  (interactive)
                  (if (use-region-p) ; Check if a region is active
                      (progn ; Execute if a region is active
                        (kill-region (region-beginning) (region-end))) ; Correctly call kill-region with start and end of the region
                    (backward-kill-word 1)))) ; Otherwise, perform kill-word-backward

;; delete char forward

(global-set-key (kbd "S-<backspace>") 'delete-forward-char)

(delete-selection-mode 1)


;; vscode-like comments

(global-set-key (kbd "C-/") 'comment-line)

;; customization

;; font

;; (set-face-attribute 'default nil
;;                     :family "Fira Code"
;;                     :height 180)



(defun is-laptop-p ()
  (let ((file-path "./.device"))
    (when (file-readable-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (string= (buffer-string) "laptop")))))

;; Example usage:
(if (is-laptop-p)
(set-frame-font "Iosevka-18" nil t)
(set-frame-font "Iosevka-16" nil t))



;; start in fullscreen

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; theme

(if (eq window-system 'w32)
    (load-theme 'jblow t)
  (load-theme 'gruber-darker t))


(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; line numbers

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode)))

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

;; Emmet

(use-package emmet-mode
  :straight t
  :config
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  )

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

;; Perl

(with-eval-after-load 'perl-mode
(add-hook 'perl-mode-hook
          (lambda ()
            (lsp)))
)

;; Perl end ===========

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

(straight-use-package
 '(prisma-mode :type git :host github :repo "pimeys/emacs-prisma-mode"))

(defun my-prisma-mode-hook ()
  (setq-local create-lockfiles nil)
  )

(add-hook 'prisma-mode-hook #'my-prisma-mode-hook)

(add-hook 'prisma-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)))


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


;; TSX

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (lsp)))

;; TSX end =========

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

(defun inhibit-electric-pair-mode-in-minibuffer (char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'inhibit-electric-pair-mode-in-minibuffer)

(require 'wrap-region)
(wrap-region-mode t)


;;; dired

(require 'dired-x)


(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")




(define-key dired-mode-map (kbd "C-<return>") 'dired-find-file-other-window)
(define-key dired-mode-map (kbd "h") 'dired-up-directory)



;; git (magit)


;; time control (wakatime)

(unless (eq window-system 'w32)
  (global-wakatime-mode)
  )


;; yasnippet
(require 'yasnippet)
(yas-global-mode)

;; company

(require 'company)

(add-to-list 'load-path "~/.emacs.d/packages/company-paths/")
(require 'company-paths)
(add-to-list 'company-backends 'company-paths)

(global-set-key (kbd "C-.") 'company-dabbrev)

(global-company-mode)

;; Number the candidates (use M-1, M-2 etc to select completions).

;; (setq company-show-numbers t)
;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay 0.1)

(global-set-key (kbd "C-p") 'company-complete-common-or-cycle)

;; no backup files

(setq make-backup-files nil)

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

;; compilation


(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setq compilation-scroll-output t)

(defun my-compilation-mode-hook ()
  "Custom compilation mode hook."
  (local-set-key (kbd "r") 'recompile)
  (local-set-key (kbd "C-x \\") 'next-error)
  (local-set-key (kbd "C-c C-c") 'kill-compilation))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)


;; Shell

(global-set-key (kbd "C-\|") 'shell-command)
(global-set-key (kbd "M-\\") 'async-shell-command)

(unless (eq window-system 'w32)
  (setq shell-file-name "/bin/bash")
  )



;; Multiple Cursors

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c")      'mc/edit-lines)
(global-set-key (kbd "C-c C->")          'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<")          'mc/mark-previous-like-this)
(global-set-key (kbd "C-:")              'mc/skip-to-previous-like-this)
(global-set-key (kbd "M-<down-mouse-1>") 'mc/add-cursor-on-click)

;; Latex for notes

(use-package auctex
  :defer    t
  :straight t
  )



(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-prettify-symbols-mode)
(add-hook 'LaTeX-math-mode-hook 'turn-on-prettify-symbols-mode)

(setq TeX-electric-sub-and-superscript t)

(require 'company)
(require 'company-math)

(add-to-list 'company-backends 'company-math-symbols-unicode)

(defun latex-binds ()
  (define-key LaTeX-mode-map (kbd "C-c C-x C-l") 'org-latex-preview))

(add-hook 'LaTeX-mode-hook 'latex-binds)


;; Latex end ==============


;; UML




(add-to-list 'load-path "~/.emacs.d/packages/plantuml-mode/")
(require 'plantuml-mode)


(setq plantuml-jar-path "~/.local/bin/plantuml.jar")
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

  (define-key org-mode-map (kbd "C-'") nil)

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
  (setq org-fontify-whole-heading-line t)

  (require 'org)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  )

(use-package org-roam
  :ensure t
  :straight t
  :custom
  (org-roam-directory (file-truename "~/Desktop/programming/notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "${title}.org" "#+title: ${title}\n")
           :unarrowed t))
        )
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))



(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; Org end ========

;; Spell

(setq-default ispell-program-name "aspell")
(setq ispell-really-aspell t)

;; Spell end ========

;; Markdown
(with-eval-after-load 'markdown-mode

  (add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/emacs-livedown"))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (emojify-mode)
              (unless (member 'company-emojify company-backends)
                (add-to-list 'company-backends 'company-emojify))))
  )
;; Markdown end ========

;; Editorconfig

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;; Editorconfig end ========

;; Tabulature ==========
(add-to-list 'load-path "~/.emacs.d/packages/tab-n-fret/")
(autoload 'chord-mode "tablature-mode" "Guitar tablature." t)
;; Tabulature end ==========

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   '(company-math-symbols-latex company-math-symbols-unicode company-paths company-bbdb company-semantic company-cmake company-capf company-clang company-files
                                (company-dabbrev company-dabbrev-code company-gtags company-etags company-keywords)
                                company-oddmuse))
 '(doc-view-continuous t)
 '(ispell-extra-args '("-d" "ru"))
 '(lsp-headerline-breadcrumb-enable nil)
 '(magit-no-confirm '(stage-all-changes unstage-all-changes set-and-push))
 '(mc/always-run-for-all t)
 '(org-support-shift-select 'always)
 '(pdf-view-midnight-colors '("#fff" . "#181818"))
 '(phi-search-additional-keybinds
   '(((kbd "C-n")
      quote phi-search-again-or-next)
     ((kbd "C-p")
      quote phi-search-again-or-previous)
     ((kbd "TAB")
      quote phi-search-yank-word)))
 '(plantuml-suppress-deprecation-warning nil)
 '(py-autopep8-command "autopep8 --ignore E501")
 '(wakatime-cli-path "~/.wakatime/wakatime-cli")
 '(wakatime-disable-on-error t)
 '(warning-suppress-types '(((org-element org-element-parser)) (comp))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
