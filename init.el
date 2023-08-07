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
