(add-to-list 'load-path "~/.emacs.d/lib/")
(load "~/.emacs.d/utils.el")
(load "~/.emacs.d/look&feel.el")
(load "~/.emacs.d/major-modes.el")
(load "~/.emacs.d/auctex-setup.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-delete-function (quote delete-char))
 '(delete-selection-mode t)
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/local/libexec/emacs/22.3/i386-apple-darwin9.5.0" "/opt/local/bin")))
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separator "")
 '(hippie-expand-try-functions-list (quote (try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(locate-command "mdfind")
 '(mumamo-chunk-coloring (quote no-chunks-colored))
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t)
 '(nxml-slash-auto-complete-flag t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
