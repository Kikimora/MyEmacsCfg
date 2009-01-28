(setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
(defun my-LaTeX-mode-hook ()
  ; make PDF by default (can toggle with C-c C-t C-p
  (TeX-PDF-mode 1)
  (turn-on-auto-fill))
(add-hook 'TeX-mode-hook 'my-LaTeX-mode-hook)
(setq-default TeX-master "main") ; All master files called "master"