(transient-mark-mode 1)
(show-paren-mode t)
(delete-selection-mode t)
;; Hide toolbar, menu, scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;;default emacs frame position
(setq initial-frame-alist
      `((left . 100) (top . 0)
        (width . 140) (height . 45)))
;;color scheme
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)
;;key bindings
(global-set-key (kbd "M-z") 'undo)
(setq mac-option-modifier 'meta)
(global-set-key "\C-x?" 'comment-region)
(global-set-key "\C-x\M-/" 'uncomment-region)
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

;;duplicate line
(defun dupline ()
  "Duplicate line under cursor"
  (interactive)
  (labels ((copy-line ()
                      (save-excursion
                        (kill-ring-save (line-beginning-position)
                                        (line-beginning-position 2))
                        (move-to-column 0)
                        (yank)))

           (copy-region ()
                        (kill-ring-save (region-beginning) (region-end))
                        (yank)))
    (let ((start-column (current-column)))
      (if (not mark-active)
            (copy-line)
          (copy-region))
      (move-to-column start-column))))
(global-set-key (kbd "C-=") 'dupline)

;; Set fonts
(when (and window-system (string-match "nt4" system-configuration))
  (set-face-font 'default "-*-Courier New-medium-r-*-*-*-*-*-*-*-*-iso8859-*")
  (setq-default line-spacing 1))

(when window-system
  (normal-erase-is-backspace-mode 1))

;; find file at point
(ffap-bindings)

;; Enable downcase-region and upcase-region commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Load `dired' itself, with `tramp' extension
(require 'dired)
(require 'dired-x)

(defun th-dired-up-directory () 
  "Go up one directory and don't create a new dired buffer but 
reuse the current one." 
  (interactive) 
  (find-alternate-file "..")) 

(defun th-dired-find-file () 
  "Find directory reusing the current buffer and file creating a 
new buffer." 
  (interactive) 
  (if (file-directory-p (dired-get-file-for-visit)) 
      (dired-find-alternate-file) 
    (dired-find-file)))

(defun th-dired-mode-init () 
  (local-set-key (kbd "^")       'th-dired-up-directory) 
  (local-set-key (kbd "RET")     'dired-find-alternate-file))

(add-hook 'dired-mode-hook 'th-dired-mode-init)

;; (add-hook 'dired-mode-hook 
;; 	  (lambda () 
;; 	    (local-set-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file 
;; 	    (local-set-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
;; 	    ))

(put 'dired-find-alternate-file 'disabled nil)
;; List directories first in dired
(require 'ls-lisp)
;; Make tramp's backup directories the same as the normal ones
(require 'tramp)
(setq tramp-backup-directory-alist backup-directory-alist)

;; Navigate the kill ring with the greatest of ease when doing M-y
(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg))
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
(ad-activate 'yank-pop)

;; no startup message
(setq inhibit-startup-message   t)
;; dont print yes print y instead
(fset 'yes-or-no-p 'y-or-n-p)
;;scrolling
(setq scroll-margin 10)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t)

(setq frame-title-format "Emacs: %b %+%+ %f")

;;ido mode
(require 'ido)
(ido-mode t)

(defun date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

;;aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)

(load "~/.emacs.d/lib/textmate.el")

(add-to-list 'load-path "~/.emacs.d/lib/w3m")
(require 'w3m-load)

(require 'idutils)

(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")