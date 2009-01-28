;; This is a way to hook tempo into cc-mode

(defvar c-tempo-tags nil
  "Tempo tags for C mode")

(defvar objc-tempo-tags nil
  "Tempo tags for Objective C mode")

(require 'tempo)

(defun tempo-complete (prompt completions match-required
                         &optional save-name no-insert)
    "Do whatever `tempo-insert-prompt' does, but use completing-read."
    (flet ((read-string (prompt)
             (completing-read prompt completions nil match-required)))
      (tempo-insert-prompt prompt save-name no-insert)))

;;insert sequence of keywords. E.g nonatomic, retain on obj-c or public final static in Java
(defun tempo-several-options (prompt completions sep save-name)
    (flet ((read-string (prompt)
			(let ((str (completing-read prompt completions)))
			  (if (string-equal str "")
			      ""
			    (let ((next-str (read-string prompt)))
			      (if (string-equal next-str "")
				  str
				(concat str sep next-str)))))))
      (tempo-insert-prompt prompt save-name)))

(add-hook 'c-mode-hook '(lambda ()
			  (local-set-key "\C-c\C-b" 'tempo-backward-mark)
			  (tempo-use-tag-list 'c-tempo-tags)))

(add-hook 'objc-mode-hook '(lambda ()
			     (local-set-key "\C-c\C-b" 'tempo-backward-mark)
			     (tempo-use-tag-list 'c-tempo-tags)
			     (tempo-use-tag-list 'objc-tempo-tags)))

(add-hook 'c++-mode-hook '(lambda ()
			    (local-set-key "\C-c\C-b" 'tempo-backward-mark)
			    (tempo-use-tag-list 'c-tempo-tags)))


;;; Preprocessor Templates (appended to c-tempo-tags)

(tempo-define-template "c-include"
		       '("#include <" r ".h>" > n
			 )
		       "incl"
		       "Insert a #include <> statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifdef"
		       '("#ifdef " (p "ifdef-clause: " clause) > n> p n
			 "#else /* !(" (s clause) ") */" n> p n
			 "#endif /* " (s clause)" */" n>
			 )
		       "ifdef"
		       "Insert a #ifdef #else #endif statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifndef"
		       '("#ifndef " (p "ifndef-clause: " clause) > n 
			 "#define " (s clause) n> p n
			 "#endif /* " (s clause)" */" n>
			 )
		       "ifndef"
		       "Insert a #ifndef #define #endif statement"
		       'c-tempo-tags)

;;; C-Mode Templates

(tempo-define-template "c-if"
		       '(> "if (" (p "if-clause: " clause) ") {" n> 
			   r n 
			   "}" > 
			   )
		       "if"
		       "Insert a C if statement"
		       'c-tempo-tags)

(tempo-define-template "c-else"
		       '(> " else {" n> 
			   r n 
			   "}" >
			   )
		       "else"
		       "Insert a C else statement"
		       'c-tempo-tags)

(tempo-define-template "c-if-else"
		       '(> "if (" (p "if-clause: " clause) ") {"  n> 
			   r n 
			   "} else {"> n>
			   r n 
			   "}" >
			   )
		       "ifelse"
		       "Insert a C if else statement"
		       'c-tempo-tags)

(tempo-define-template "c-while"
		       '(> "while (" (p "while-clause: " clause) ") {" n> 
			   r n 
			   "}">
			   )
		       "while"
		       "Insert a C while statement"
		       'c-tempo-tags)

(tempo-define-template "c-for"
		       '(> "for (" p "; " p "; " p") {"  n> 
			   r n 
			   "}" >
			   )
		       "for"
		       "Insert a C for statement"
		       'c-tempo-tags)

(tempo-define-template "c-for-i"
		       '(> "for (int " p " = 0; " p " < " p "; ++"p") {" n>
			   r n 
			   "}" >
			   )
		       "fori"
		       "Insert a C for loop: for(x = 0; x < ..; x++)"
		       'c-tempo-tags)

(tempo-define-template "c-switch"
		       '(> "switch (" p ") {" n>
			   r n
			   "}" >
			   )
		       "switch"
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-case"
		       '(> "case " p ":" > n> p n>
			   "break;"
			   )
		       "case"
		       "Insert a C case statement"
		       'c-tempo-tags)

(tempo-define-template "objc-interface"
		       '("@interface " p " {" n n
			 "}" n n r n n
			 "@end"
			 )
		       "in"
		       "Insert a class skeleton"
		       'objc-tempo-tags)

(tempo-define-template "objc-selector"
		       '(> "@selector(" p ")")
		       "sel"
		       "Insert @selector()"
		       'objc-tempo-tags)

(tempo-define-template "objc-synthesize"
		       '(>"@synthesize " p ";")
		       "syn"
		       "Insert @synthesize"
		       'objc-tempo-tags)

(tempo-define-template "objc-private-interface"
		       '(> "@interface " p " ()" n>
			   n p n n
			   "@end" >
			   )
		       "pin"
		       "Insert private interface"
		       'objc-tempo-tags)

(tempo-define-template "objc-impl"
		       '("@implementation " r  n n
			 p
			 n n
			 "@end"
			 )
		       "impl"
		       "Insert a class skeleton"
		       'objc-tempo-tags)

(tempo-define-template "objc-instance-method-def"
		       '("- ("r")" p " {"n
			 > p n
			 "}"
			 )
		       "mi"
		       "Insert instance method definition"
		       'objc-tempo-tags)

(tempo-define-template "objc-class-method-def"
		       '("+ (" p ")" p " {"n
			 r n
			 "}"
			 )
		       "mc"
		       "Insert class method definition"
		       'objc-tempo-tags)

(tempo-define-template "objc-property"
		       '(>"@property (" p ") " p " *"r ";" n>
			 )
		       "prop"
		       "Insert a method definition"
		       'objc-tempo-tags)

(tempo-define-template "objc-import"
		       '("#import \"" r ".h\""
			 )
		       "imp"
		       "Insert a #import \"\" statement"
		       'objc-tempo-tags)

(tempo-define-template "objc-std-import"
		       '("#import <" r ".h>"
			 )
		       "impc"
		       "Insert a #import <> statement"
		       'objc-tempo-tags)

(tempo-define-template "objc-foreach"
		       '(> "for (" (p "var: " vars) " in " (p "collection: " col) ") {"  n> 
			   r n 
			   "}" >
			   )
		       "foreach"
		       "Insert a C for statement"
		       'objc-tempo-tags)
