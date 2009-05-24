(global-set-key (kbd "M-z") 'undo)
(setq mac-option-modifier 'meta)
(global-set-key "\C-x?" 'comment-or-uncomment-region)
(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\C-x\C-m" 'compile)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key [f3] 'next-error)
(global-set-key [f4] (lambda () (interactive) (next-error -1)))
(global-set-key "\C-z" 'shell)
(global-set-key "\C-c\C-m" (lambda ()
			     (interactive)
			     (let ((cmd (concat "open \"" (buffer-file-name) "\"")))
			       (shell-command cmd ()))))


(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-up>") 'shrink-window)

;;aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)

(global-set-key (kbd "C-=") 'dupline)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)
