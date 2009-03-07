(load "haskell-site-file.el")
(load "~/.emacs.d/docsetview.el")
(load "~/.emacs.d/lib/objc-c-mode.el")
(load "~/.emacs.d/lib/js2.el")

(setq auto-mode-alist (append '(("\\.h$" . objc-mode) ("\\.m$" . objc-mode) ("\\.js$" . js2-mode)) auto-mode-alist))

(font-lock-add-keywords 'objc-mode
    ;;fontify Objc 2.0 keywords
  '(("\\<\\(@required\\|@synthesize\\|@optional\\|@property\\|@defs\\|YES\\|NO\\)\\>" . font-lock-keyword-face)
    ;;[myObject <'methodWithArg:'> arg]
    ("\\<\\w+:" . font-lock-function-name-face)
    ;;[NSFileManager <'defaultManager'>]
    ("\\[\\w+ \\(\\w+\\)\\]" . (1  font-lock-function-name-face))
    ;;[[NSFileManager defaultManager] <'someMethod'>]
    ("\\] \\(\\w+\\)" . (1  font-lock-function-name-face)))) 

(defun find-one-of (candidates name)
  (if candidates
      (let ((candidate-name (concat (file-name-sans-extension name) "." (car candidates))))
	(message candidate-name)
	(if (file-exists-p candidate-name)
	    (find-file candidate-name)
	  (find-one-of (cdr candidates) name)))))
  
(defun c-switch-to-header ()
  "Switch to the header file from source file."
  (interactive)
  (when (string-match "\\([a-zA-Z0-9\-\_\/]+\\)\\.\\(.*?\\)" (buffer-name))
    (let ((filename (buffer-file-name)))
      (if (string-match ".*\\.\\(h\\|hxx\\|hpp\\)$" (buffer-name))
	  (find-one-of (list "c" "m" "mm" "c++" "cpp" "cxx") (buffer-name))
	(find-one-of (list "h" "hxx" "hpp") (buffer-name))))))

(defun my-c-mode-common-hook ()
  (setq compilation-window-height 10)
  (setq c-basic-offset 4 indent-tabs-mode nil)
  (setq tab-width 4)
  (textmate-mode)
  (c-subword-mode)
  (local-set-key "\C-ch" 'c-switch-to-header)
  (local-set-key "\M-/" (make-hippie-expand-function
			 '(try-expand-dabbrev-visible
			   try-expand-dabbrev
			   try-expand-dabbrev-all-buffers
			   dsv-try-expand-from-doc) t)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-objc-mode-hook ()
  (local-set-key "\C-hs" 'dsv-display-doc)
  (local-set-key "\C-\M-]" 'objc-wrap-brackets)
  (set (make-local-variable 'compile-command) "~/.emacs.d/tools/build-xcode-proj.sh")
  (ad-enable-advice 'compilation-handle-exit 'after 'visit-tags-after-compilation))
(add-hook 'objc-mode-hook 'my-objc-mode-hook)

(defadvice compilation-handle-exit (after visit-tags-after-compilation pre act)
  (if (get-buffer "TAGS")
      (kill-buffer "TAGS")))

(ad-disable-advice 'compilation-handle-exit 'after 'visit-tags-after-compilation)

(defun objc-wrap-brackets (&optional count)
  (interactive "p")
  (backward-up-list count)
  (insert "[")
  (forward-sexp +1)
  (save-excursion (insert "]"))
  (just-one-space))

(defun set-project-path ()
  (interactive)
  (let ((project (read-from-minibuffer "Project directory: " default-directory)))
    (setenv "PROJ_PATH" project)))

(defun my-nxml-mode-hook ()
  (local-set-key "\M-/" 'nxml-complete))
(add-hook 'nxml-mode-common-hook 'my-nxml-mode-common-hook)