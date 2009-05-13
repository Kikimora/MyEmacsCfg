(shell-command "source ~/.env_vars")

(add-to-list 'load-path "~/.emacs.d/lib/")
(add-to-list 'load-path "~/.emacs.d/lib/color-theme")

(add-to-list 'load-path "~/.emacs.d/lib/git-emacs")
(require 'git-emacs)

(load "~/.emacs.d/utils.el")
(load "~/.emacs.d/look&feel.el")
(load "~/.emacs.d/major-modes.el")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-delete-function (quote delete-char))
 '(delete-selection-mode t)
 '(exec-path (quote ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/local/libexec/git-core" "/opt/local/bin")))
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separator "")
 '(hippie-expand-try-functions-list (quote (try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(html-script-regions (quote (("<%" "%>" visual-basic-mode) ("<\\?" "\\?>" php-mode c-mode) ("<style[ 	]+type=\"text/css\"" "</style>" css-mode c-mode) ("<style[ 	]+type=\"css\"" "</style>" css-mode c-mode) ("<script[ 	]+language=\"vbscript\"[ 	]*>" "</script>" visual-basic-mode) ("<script[ 	]+type=\"text/vbscript\"[ 	]*>" "</script>" visual-basic-mode) ("<script[ 	]+language=\"javascript\"[ 	]*>" "</script>" js2-mode javascript-mode ecmascript-mode javascript-generic-mode c-mode) ("<script[ 	]+type=\"text/javascript\"[ 	]*>" "</script>" js2-mode javascript-mode ecmascript-mode javascript-generic-mode c-mode))))
 '(ido-use-filename-at-point (quote guess))
 '(locate-command "mdfind")
 '(mumamo-chunk-coloring (quote no-chunks-colored))
 '(nxhtml-default-encoding (quote utf-8))
 '(nxhtml-skip-welcome t)
 '(nxml-slash-auto-complete-flag t)
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode nil)
 '(show-paren-mode t)
 '(transient-mark-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
