(defun string-trim (bag s)
  "Returns a substring of the string specified by S that has had every
character in BAG removed from the beginning and end.  S must be a string or a
symbol.  If S is a symbol, its print name is used as the string.  The BAG
argument may be any sequence of characters.  Characters are trimmed from the
beginning and from the end of S until the first character not in BAG is
found."
  (let* ((len (length s))
	 (i1  (string-left-trim-index bag s 0 len))
	 (i2  (string-right-trim-index bag s len)))
    (if (<= i2 i1) "" (substring s i1 i2))))

(defun string-left-trim (bag s)
  "Returns a substring of the string specified by S that has had every
character in BAG removed from the beginning.  S must be a string or a symbol.
The BAG argument may be any sequence of characters.  Characters are trimmed
from the beginning of S until the first character not in BAG is found."
  (let* ((len (length s))
	 (i1  (string-left-trim-index bag s 0 len)))
    (if (<= len i1) "" (substring s i1 len))))


(defun string-right-trim (bag s) 
  "Returns a substring of the string specified by S that has had every
character in BAG removed from the end.  S must be a string or a symbol.
The BAG argument may be any sequence of characters.  Characters are trimmed
from the end of S until the first character not in BAG is found."
  (let ((i2 (string-right-trim-index bag s (length s))))
    (if (<= i2 0) "" (substring s 0 i2))))


(defun string-left-trim-index (bag s i uplim)
  (if (or (eql i uplim)
	  (not (member (schar s i) bag)))
    i
    (string-left-trim-index bag s (1+ i) uplim)))


(defun string-right-trim-index (bag s i)
  (if (or (eql i 0)
	  (not (member (schar s (1- i)) bag)))
    i
    (string-right-trim-index bag s (1- i))))


(defun schar (s i)
  "Returns the ITH character of string S as a character object.  S must be
a simple string or a symbol.  If S is a symbol, its print name is used
as the string to operate on.  I must be a non-negative integer less than the
length of the string (indexing is zero-origin).  The function schar applied
to simple strings behaves identically to aref or char, but it may be faster
than either in many implementations."

  (string-to-char (substring s i)))

(defun he-tag-beg ()
  (save-excursion 
    (backward-word 1)
    (point)))


(defun forward-while (pred)
  (message "1")
  (setq line (trimmed-line))
  (message "2")
  (while (funcall pred line)
    (message "3")
    (setq line (if (= (forward-line 1) 0)
		   (trimmed-line)
		 nil)))
  line)


(defun trimmed-line ()
  (string-trim '(?\ ?\t ?\n) (thing-at-point 'line)))