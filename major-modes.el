;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;js2 mode
(add-to-list 'load-path "~/.emacs.d/lib/js2/build")
(autoload 'js2-mode "js2" nil t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;c mode + customizations + various helper minor modes
(load "~/.emacs.d/docsetview.el")
(autoload 'objc-mode "objc-c-mode" nil t)

(autoload 'csharp-mode "csharp-mode" nil t)
(autoload 'php-mode "php-mode" nil t)
(require 'html-script)
(require 'etags-select)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

(font-lock-add-keywords 'objc-mode
    ;;fontify Objc 2.0 keywords
  '(("\\<\\(@required\\|@synthesize\\|@optional\\|@property\\|@defs\\|YES\\|NO\\)\\>" . font-lock-keyword-face)
    ;;[myObject <'methodWithArg:'> arg]
    ("\\<\\w+:" . font-lock-function-name-face)
    ;;[NSFileManager <'defaultManager'>]
    ("\\[\\(?:\\w\\.?\\)+ \\(\\w+\\)\\]" . (1  font-lock-function-name-face))
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
  (let ((file-name (buffer-file-name)))
    (when (string-match "\\([a-zA-Z0-9\-\_\/]+\\)\\.\\(.*?\\)" file-name)
      (if (string-match ".*\\.\\(h\\|hxx\\|hpp\\)$" file-name)
	  (find-one-of (list "c" "m" "mm" "c++" "cpp" "cxx") file-name)
	  (find-one-of (list "h" "hxx" "hpp") file-name)))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;haskell mode
(load "~/.emacs.d/lib/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(put 'dired-find-alternate-file 'disabled nil)
;; List directories first in dired
(require 'ls-lisp)
;; Make tramp's backup directories the same as the normal ones
(require 'tramp)
(setq tramp-backup-directory-alist backup-directory-alist)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;w3m
(setq w3m-command "/opt/local/bin/w3m")
(require 'w3m)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(autoload 'magit-status "magit" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure-mode
(require 'clojure-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swank-clojure
(require 'swank-clojure-autoload)
(swank-clojure-config
 (setq swank-clojure-binary "~/.emacs.d/tools/clojure"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))
(require 'slime)
(slime-setup)

(defun myslime ()
  (interactive)
  (let ((proj (getenv "PROJ_PATH")))
    (when proj
      (setenv "CLOJURE_EXTRA_CP"
	    (reduce (lambda (str acc) (concat acc ":" str))
		    (remove-if (lambda (p) (or (string-match "javadoc" p) (string-match "sources" p)))
			       (directory-files (concat proj "lib") t ".jar$"))))))
  (slime))

(defun clj-import (re)
  (interactive "sClass regex?: ")
  (slime-eval-with-transcript
   `(swank:interactive-eval ,(format "(user/find-classes #\"%s\")" re))
   nil t
   `(lambda (s)
      (with-current-buffer ,(current-buffer)
        (let ((classes (car (read-from-string s))))
          (insert (clj-format-import classes)))))))

(defun clj-format-import (classes)
  (let ((packages (make-hash-table :test 'equal)))
    (mapc (lambda (class)
            (let ((pkg (file-name-directory class))
                  (name (file-name-nondirectory class)))
              (puthash pkg (cons name (gethash pkg packages '()))
                       packages )))
          classes)
    (let ((imports '()))
      (maphash (lambda (pkg names)
                 (push (format "'(%s %s)"
                               (replace-regexp-in-string
                                "/" "."
                                (replace-regexp-in-string "/$" "" pkg))
                               (mapconcat 'identity names " "))
                       imports))
               packages)
      (format "(import %s)\n"
              (mapconcat 'identity imports "\n        ")))))
