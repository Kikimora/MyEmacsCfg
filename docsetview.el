;;DocSet documentation lookup and hippie expand completion function
(defvar dsv-docset-path "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone3_0.iPhoneLibrary.docset/Contents/Resources/docSet.dsidx")

(defvar dsv-program-path "~/.emacs.d/tools/docsetview")

(defun dsv-invoke-docview-util (str op)
  (with-output-to-temp-buffer "*ObjcDocCompletions*"
    (call-process dsv-program-path nil "*ObjcDocCompletions*" nil op str dsv-docset-path))
  (delete-other-windows))

(defun dsv-completions (str)
  (dsv-invoke-docview-util str "complete")
  (set-buffer "*ObjcDocCompletions*")
  (setq lines (count-lines (point-min) (point-max)))
  (setq result '())
  (while (= (forward-line 1) 0)
    (setq result (cons (trimmed-line) result)))
  result)

(defun dsv-display-doc-in-w3m (sym)
  (dsv-invoke-docview-util sym "docurl")
  (set-buffer "*ObjcDocCompletions*")
  (w3m-browse-url (trimmed-line)))


(defun dsv-display-doc ()
  (interactive)
  (dsv-display-doc-in-w3m
   (completing-read "Describe iPhone SDK symbol: "
		    (dynamic-completion-table dsv-completions)
		    nil
		    nil
		    (thing-at-point 'word))))

(defun dsv-try-expand-from-doc (old)
  (unless old
    (he-init-string (he-tag-beg) (point))
    (dsv-invoke-docview-util he-search-string "complete"))
  (let ((completion (save-excursion
		      (set-buffer "*ObjcDocCompletions*")
		      (forward-while (lambda (line)
				       (if (> (length line) 0)
					   (he-string-member line he-tried-table)					      
					 nil))))))
    (if (> (length completion) 0)
	(progn
	  (setq he-tried-table (cons completion he-tried-table))
	  (he-substitute-string completion t)
	  completion)
      (progn
	(he-reset-string)
	(kill-buffer "*ObjcDocCompletions*")
	nil))))