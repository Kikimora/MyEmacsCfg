(transient-mark-mode 1)
(show-paren-mode t)
(delete-selection-mode t)
;; Hide toolbar, scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(fringe-mode 'none)

;;default emacs frame position
(setq initial-frame-alist
      `((left . 100) (top . 0)
        (width . 140) (height . 45)))
;;color scheme
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)

;; Set fonts
(when (and window-system (string-match "nt4" system-configuration))
  (set-face-font 'default "-*-Courier New-medium-r-*-*-*-*-*-*-*-*-iso8859-*")
  (setq-default line-spacing 1))

(when window-system
  (normal-erase-is-backspace-mode 1))

;; Enable downcase-region and upcase-region commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; no startup message
(setq inhibit-startup-message   t)
;; dont print yes print y instead
(fset 'yes-or-no-p 'y-or-n-p)
;;scrolling
(setq scroll-margin 10)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t)

(setq frame-title-format "Emacs: %b %+%+ %f")

(setq backup-directory-alist
      (cons '("." . "~/.emacs-backups") backup-directory-alist))

(setq visible-bell t)