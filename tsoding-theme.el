(deftheme tsoding
  "My recreation of tsoding's theme")

(custom-theme-set-faces
 'tsoding
 '(highlight ((t (:background "#FFF" :foreground "#000"))))
 '(font-lock-comment-face ((t (:foreground "#FF9B21"))))
 '(font-lock-doc-face ((t (:foreground "#FF9B21"))))
 '(font-lock-function-name-face ((t (:foreground "#96a6c8"))))
 '(font-lock-keyword-face ((t (:foreground "#FFDD33" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-constant-face))))
 '(font-lock-string-face ((t (:foreground "#79BF46"))))
 '(font-lock-type-face ((t (:foreground "#949bb0" :slant normal))))
 '(mode-line-buffer-id ((t (:foreground "#FFF" :weight normal))))
 '(isearch ((t (:inherit region :background "#FFF" :foreground "#000"))))
 '(isearch-fail ((t (:inherit isearch :background "#F92672" :foreground "#FFF" :weight normal))))
 '(match ((t (:background "#FFF" :foreground "#000" :weight normal))))
 '(next-error ((t (:foreground "Red"))))
 '(query-replace ((t (:inherit (isearch)))))
 '(cursor ((t (:background "#FFDD33"))))
 '(mode-line ((t (:background "#252525" :foreground "#f8f8f8" :box (:line-width (1 . -1) :color "#252525" :style flat-button)))))
 '(mode-line-emphasis ((t (:weight normal))))
 '(minibuffer-prompt ((t (:foreground "#949bb0"))))
 '(homoglyph ((t (:foreground "#949bb0"))))
 '(escape-glyph ((t (:foreground "#949bb0"))))
 '(error ((t (:foreground "Red" :weight bold))))
 '(warning ((default (:weight bold)) (((class color) (min-colors 16)) (:foreground "DarkOrange")) (((class color)) (:foreground "yellow"))))
 '(success ((default (:weight bold)) (((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "Green1")) (((class color) (min-colors 16) (background dark)) (:foreground "Green")) (((class color)) (:foreground "green"))))
 '(font-lock-variable-name-face ((t (:foreground "#FFF"))))
 '(region ((t (:extend nil :background "#504945"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#252525" :foreground "#f8f8f8" :box (:line-width (1 . -1) :color "#252525")))))
 '(font-lock-constant-face ((t (:foreground "#95A99F"))))
 '(font-lock-builtin-face ((t (:foreground "#ffdd33"))))
 '(line-number-current-line ((t (:inherit default :foreground "#ffdd33"))))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#181818" :foreground "#FFF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 181 :width normal :foundry "CTDB" :family "Fira Code")))))

(provide-theme 'tsoding)
