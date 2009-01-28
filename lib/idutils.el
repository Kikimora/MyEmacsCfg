;;; idutils.el -- emacs interface to `lid -R grep', a.k.a. `gid'
;;; Copyright (C) 1995, 1996, 2006, 2007 Free Software Foundation, Inc.
;;; Greg McGary <[EMAIL PROTECTED]>.

;; This file is part of GNU idutils.

;; GNU idutils is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU idutils is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; This package provides the tools meant to help editing PO files,
;;; as documented in the GNU idutils user's manual.  See this manual
;;; for user documentation, which is not repeated here.

;;; To install, merely put this file somewhere GNU Emacs will find it,
;;; then add the following lines to your .emacs file:
;;;
;;;   (autoload 'gid "idutils")
;;;
;;; You may also adjust some customizations variables, below, by defining
;;; them in your .emacs file.

(require 'compile)
(require 'thingatpt)

(defvar gid-command "gid32" "The command run by the gid function.")

(defvar gid-mode-font-lock-keywords
  '(("^\\(Compilation\\|Gid\\) started.*"
     (0 '(face nil message nil help-echo nil mouse-face nil) t))))

(define-compilation-mode gid-mode "Gid"
  "Specialization of compilation-mode for use with gid."
  nil)

(defun gid (args)
  "Run gid, with user-specified ARGS, and collect output in a buffer.
While gid runs asynchronously, you can use the \\[next-error] command to
find the text that gid hits refer to. The command actually run is
defined by the gid-command variable."
  (interactive (list (read-input
     (concat "Run " gid-command " (with args): ") (thing-at-point 'symbol))))
  (let (compile-command
        (compilation-error-regexp-alist grep-regexp-alist)
        (compilation-directory default-directory))
    (save-some-buffers (not compilation-ask-about-save) nil)
    (let ((cmd (if (getenv "PROJ_PATH")
		   (concat "cd " (getenv "PROJ_PATH") " && " gid-command)
		 gid-command)))
      (compilation-start (concat cmd " " args) 'gid-mode nil
			 (regexp-quote args)))))

(provide 'idutils)