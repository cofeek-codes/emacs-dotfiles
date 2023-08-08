;;binds

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)

;; move between panes with S-<arrows>

(windmove-default-keybindings)


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
     ```



;; disable sound (annoyng)

     (setq visible-bell t)
     (setq ring-bell-function 'ignore)





;; package management

(unless (package-installed-p 'use-package)
  (package-refresh-contents) 
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
 ;; (package-refresh-contents) don't want it to refresh every time



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

;; lsp
