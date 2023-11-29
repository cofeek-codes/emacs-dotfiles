;; duplicate-line-mode.el

(define-minor-mode duplicate-line-mode
  "Duplicate Line Mode"
  :init-value nil
  :lighter " dl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<up>") 'duplicate-line)
            (define-key map (kbd "<down>") 'duplicate-line)
            (define-key map (kbd "RET") 'toggle-duplicate-line-mode)
            map))





(defun toggle-duplicate-line-mode ()
  "Toggle Duplicate Line Mode"
  (interactive)
  (if duplicate-line-mode
      (progn
        (duplicate-line-mode -1)
        (run-hooks 'duplicate-line-mode-exit-hook))
    (progn
      (duplicate-line-mode 1))))

(add-hook 'duplicate-line-mode-exit-hook
          (lambda ()
            (message "Duplicate-Line mode disabled in current buffer")))

;; duplicate-line

(defun duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (forward-line)
    (insert current-line)
    (forward-line -1)))

(provide 'duplicate-line-mode)
