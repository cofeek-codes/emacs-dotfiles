(deftheme jblow
  "My version of Jhonathan Blow emacs theme")

(custom-theme-set-faces
 'jblow
 '(region ((t (:extend nil :background "#0000ff"))))
 '(cursor ((t (:background "#ffffff"))))
 '(fringe ((t (:background "#062329" :foreground "#ffffff"))))
 '(highlight ((t (:foreground "#0000ff"))))
 '(font-lock-keyword-face ((t (:foreground "#ffffff"))))
 '(font-lock-type-face ((t (:foreground "#8cde94"))))
 '(font-lock-constant-face ((t (:foreground "#7ad0c6"))))
 '(font-lock-variable-name-face ((t (:foreground "#c1d1e3"))))
 '(font-lock-builtin-face ((t (:foreground "#ffffff"))))
 '(font-lock-string-face ((t (:foreground "#2ec09c"))))
 '(font-lock-comment-face ((t (:foreground "#44b340"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "#ffffff"))))
 '(font-lock-preprocessor-face ((t (:foreground "#8cde94"))))
 '(warning ((t (:foreground "#ffaa00" :weight bold))))
 '(font-lock-warning-face ((t (:foreground "#ffaa00"))))
 '(trailing-whitespace ((t (:background "#ffaa00"))))
 '(mode-line-buffer-id ((t (:distant-foreground "#d1b897" :foreground "#062329" :weight bold))))
 '(mode-line ((t (:background "#d1b897" :foreground "#062329" :box (:line-width (1 . -1) :style released-button)))))
 '(mode-line-inactive ((t (:weight normal :box (:line-width (1 . -1) :color "grey40") :foreground "#062329" :background "#dedede" :inherit mode-line))))
 '(line-number ((t (:background "#062329" :foreground "#126367"))))
 '(line-number-current-line ((t (:background "#062329" :foreground "#ffffff"))))
 '(ido-subdir ((t (:inherit font-lock-string-face))))
 '(show-paren-match ((t (:background "#0000ff"))))
 '(lsp-face-highlight-textual ((t (:inherit region))))
 '(company-tooltip ((t (:background "#083a41"))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#041a29"))))
 '(company-tooltip-scrollbar-track ((t (:background "#062329"))))
 '(company-tooltip-selection ((t (:background "#0a4a59" :foreground "#ffffff"))))
 '(magit-section-highlight ((t (:extend t :background "#0a4a59"))))
 '(magit-blame-heading ((t (:inherit magit-blame-highlight :extend t :slant normal :weight normal))))
 '(magit-blame-highlight ((t (:extend t :background "#0a4a59" :foreground "#ffffff"))))
 '(magit-diff-hunk-heading-highlight ((t (:extend t :background "#083a41" :foreground "#ffffff"))))
 '(magit-section-heading ((t (:extend t :foreground "#ffffff" :weight bold))))
 '(magit-diff-hunk-heading ((t (:extend t :background "#083a41" :foreground "#ffffff"))))
 '(header-line ((t (:inherit mode-line :background "#083a41" :foreground "#ffffff" :box nil))))
 '(magit-diff-context-highlight ((t (:extend t :background "#0a4a59" :foreground "#ffffff"))))
 '(lsp-face-highlight-read ((t (:inherit region :underline t))))
 '(compilation-error ((t (:inherit (error)))))
 '(error ((t (:foreground "tomato" :weight bold))))
 '(success ((t (:foreground "ForestGreen" :weight bold))))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#062329" :foreground "#d1b897" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 211 :width normal :foundry "UKWN" :family "Iosevka")))))

(provide-theme 'jblow)
