;;
;; jtags.el - Provide enhanced tags functionality Java development.
;;
;; Authors:                 Alexander Baltatzis (alexander@baltatzis.com)
;;                         Johan Dykström (johan.dykstrom@relogic.se)
;; Created:                May 2001
;; Version:                0.9
;; Keywords:               java, tags, etags

;; Copyright (C) 2001 Alexander Baltatzis.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Commentary:
;;
;; The main purpose of this package is to provide an improved tags lookup
;; function compared to the ordinary etags package. The improved tags lookup 
;; function is only implemented for for Java source files. The following 
;; public functions are defined within the package:
;;
;; (defun jtags-find-tag ()
;; (defun jtags-member-completion ();
;;
;; Installation:
;;
;; Place jtags.el in your load.path and place the following line in
;; the appropriate init file:
;;
;; (require 'jtags)
;;
;; Configuration:
;;
;; Create tags table files for your Java source code files (you need
;; to expand src.jar). Use the etags program that comes with the emacs
;; distribution, for example using the following command:
;;
;; windows:
;; dir /b /s *.java | etags --members --language=java -
;;
;; unix:
;; find `pwd` | egrep java$ |  etags --members --language=java -
;;
;; Include the tags table files in your tags-table-list in the
;; appropriate init file, for example like this:
;;
;; (setq tags-table-list '("c:/Programs/Java/jdk1.3/src" "c:/Work/src"))
;;
;; If you want to browse doc files in your browser, do set
;; jtags-javadoc-root to the folder where the javadoc generated
;; index.html resides, for example like this:
;;
;; (setq jtags-javadoc-root "c:/Programs/Java/jdk1.3/docs/api")

;; Structure of this file:
;;
;; Global variables:
;; 
;; 
;; Main functions:
;; 
;; 
;; Generic functions:
;; 
;; 
;; Functions for finding which class(es) the point is in:
;; 
;; 
;; Functions for finding base classes for a list of classes:
;; 
;; 
;; Functions for looking up tags:
;; 
;; 
;; Functions for looking up completions:
;; 
;; 
;; Functions that operate on the tagfile
;; 
;; 
;; Functions that parses javafiles
;; 
;; 
;; HTML functions:
;; 
;; 
;; function test
;;
;; 

;; ----------------------------------------------------------------------------
;; Global variables 
;; ----------------------------------------------------------------------------

(require 'etags)
(require 'cl)

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Variable to distinguish which emacs is running. t if Xemacs nil if GNU Emacs"
  )

(defvar jtags-file
  (if (string-match "jtags.el" (buffer-file-name))
      (buffer-file-name)
    load-file-name)
  "Physical location of jtags.el")
  
(defvar jtags-javadoc-frameset
  (let (frameset-file)
    ;; try create frameset-file in same directory as jtags package
    (if (and jtags-file (file-exists-p jtags-file))
	(setq frameset-file (concat (file-name-directory jtags-file) "javadoc_frameset.html")))
    (if (and frameset-file (file-writable-p frameset-file))
	frameset-file             ;; create frameset in jtags package dir
      "~/javadoc_frameset.html")  ;; create frameset in home directory 
    )
  "File name of javadoc frameset. Included with jtags.el is a frameset
file. This will be altered in order to give the fancy javadocs frames"
  )

(defvar jtags-javadoc-root '("D:\\Utv\\jdk1\.3\.1\\docs\\api" "as" "D:\\Utv\\jdk1\.3\.1\\jaxp-1\.1\\docs\\api")
  "*User defined variable to be set at javadoc root"
  )

(defvar jtags-last-possible-completions nil
  "Holds a list of the text to be replaced and possible completions. This variable is
used to circle completions"
  )


(defvar jtags-trace-on nil
"*Set to true to get my-trace-message to actually print things.")

(defun jtags-message (&rest args)
  (when jtags-trace-on
    (apply 'message args)))


;; ----------------------------------------------------------------------------
;; Main functions:
;; ----------------------------------------------------------------------------

(defun jtags-find-tag ()
  "Looks up the identifier in the marked expression or where the point is 
positioned, loads the file where the identifier is defined and places the 
point on the line of the definition.

This is one of the main function in this package and could preferrably
be bound to a key. Example:

\(global-set-key \[\(meta ,\)\] 'jtags-find-tag\)"
  (interactive)
  (let* ((orig-buffer (buffer-name))
	 (identifiers (parse-java-line))
	 (classes (jtags-get-class-list))
	 (definition (jtags-recursive-tags-lookup identifiers classes))
	 )

    (jtags-message "in jtags-find-tag: jtags-recursive-tags-lookup returned %s" definition)
    ;; If we did not find anything, check if first identifier is a local 
    ;; variable
    (if (null definition)
	(setq definition (jtags-local-tags-lookup identifiers classes)))

    ;; If we found a definition in the tags table, load the file and go to
    ;; the first line of the definition
    (if (null definition)
	(message "Tag not found!")
      (find-file (car definition))
      (goto-line (cadr definition))
      (message "Found %s in %s at line %d" (caddr definition) (car definition) (cadr definition))
      ;; Check doc path
      (when jtags-javadoc-root
	(when (jtags-browse-url definition)
	  (message "browse succeded returning to %s" orig-buffer)
	  (switch-to-buffer orig-buffer)
	  ))
      )
    definition   ;; need to return true or false for automatic tests
    ))

;; (setq list '("aaa" "bbb" "ccc"))
;; (setq list (append list '("dddd" "ee")))
;; (char-equal ?/ (elt jtags-javadoc-root (1- (length jtags-javadoc-root))))
;; (setq jtags-javadoc-root "d:\\utv\\jdk1\.2\.2\\docs\\api")
;; (stringp jtags-javadoc-root)
;; (setq package '("java.lang"))
;; (setq classdoc (concat (car package) "/"))

(defun jtags-member-completion ()
  "Looks up the identifier in the marked expression or where the point is 
positioned and tries to complete word by looking up all members in class

This is one of the main function in this package and could preferrably
be bound to a key. Example:

\(global-set-key \[\(meta ,\)\] 'jtags-find-tag\)

This method could be replaced and integrated to dabbrev-expand instead."

  (interactive)
  (let (orig_pos p)
    
    ;; On sequal calls, cycle through the possible completions
    (if (eq last-command 'jtags-member-completion)
	(progn
	  (when jtags-last-possible-completions
	    ;; shift list one step if more than one member
	    (setq p (cdr jtags-last-possible-completions))
	    (when (cdr p) ;; more than one member
	      (setcar jtags-last-possible-completions (cadr jtags-last-possible-completions)) 
	      (while (cdr p)
		(setq p (cdr p)))
	      (setcdr p (list (cadr jtags-last-possible-completions))) 
	      (setq jtags-last-possible-completions (cons (car jtags-last-possible-completions) (cddr jtags-last-possible-completions)))
	      )))
      
      ;; reset previous completions
      (setq jtags-last-possible-completions nil)
      
      ;; Find all possible completions and the text to be completed.
      (setq jtags-last-possible-completions (jtags-find-member-completions))
      )      
    (if jtags-last-possible-completions
	(progn
	  
	  ;; replace text in buffer
	  (unless (and (= (length jtags-last-possible-completions) 2)
		       (eq last-command 'jtags-member-completion))
	    (setq orig_pos (point))
	    (backward-char (length (car jtags-last-possible-completions)))
	    (search-forward (car jtags-last-possible-completions) nil t)
	    (replace-match (cadr jtags-last-possible-completions) t)
	    (goto-char orig_pos)
	    )
	  
	  ;; Message
	  (if (= (length jtags-last-possible-completions) 2)
	      (message "Found one completion -> %s" (cadr jtags-last-possible-completions))
	    ;; Find shortkut key
	    (let ((key (where-is-internal 'jtags-member-completion nil t)))
	      (if key
		  (setq key (concat "cycle with " (key-description key)))
		;;(setq key "M-x jtags-member-completion RET") ;; TODO destroys last-command
		(setq key "")
		)
	      (message "Found %d completions -> %s %s" (1- (length jtags-last-possible-completions)) (cdr jtags-last-possible-completions) key)))
	  )
      (message "No completion!")
    )))


(defun jtags-browse-url (definition)
  "Checks if definition exists in jtags-javadoc-root and if so, builds
the javadoc html documentation by building up a frameset and start
system browser"
  (let ((package (find-packages "package")) classdoc found-classdoc list (javadoc-root-list jtags-javadoc-root)
	(start 0))
    (when (and package jtags-javadoc-root)
      (if (stringp javadoc-root-list)
	  (setq javadoc-root-list (list jtags-javadoc-root)))
      (mapcar (lambda (javadoc-root)   
		;; change \ to / in javadoc-root
		;;(if (= (string-match "\\\\\\\\" javadoc-root) 0) (setq start 2)) ;; skip win network names \\
		(jtags-message "in jtags-browse-url: checking package = %s with %s" (car package) javadoc-root)
		(while (setq start (string-match "\\\\" javadoc-root start))
		  (aset javadoc-root start ?/))
		;; set a / at end
		(unless (char-equal ?/ (elt javadoc-root (1- (length javadoc-root))))
		  (setq javadoc-root (concat javadoc-root "/")))
		;; change . to / in package		
		(setq start 0)
		(while (setq start (string-match "\\\." (car package) start))
		  (aset (car package) start ?/))
		(setq classdoc (concat javadoc-root (car package) "/" (caddr definition) ".html"))

		(jtags-message "in jtags-browse-url: checking for javadoc file %s" list classdoc)
		(when (file-regular-p classdoc)
		  (jtags-message "in jtags-browse-url: Found file %s" classdoc)
		  (setq found-classdoc (cons classdoc javadoc-root))
		  ))
	      javadoc-root-list)
      (when found-classdoc
	(jtags-message "in jtags-browse-url: Launching browser with file %s in %s"
		       (car found-classdoc) (cdr found-classdoc))
	(jtags-prepare-javadoc-frameset (car found-classdoc) (cdr found-classdoc))
	(browse-url jtags-javadoc-frameset))
      )))

;;D:\Utv\jdk1.3.1\docs\api\java\lang\String.html
;;(browse-url  "D:/Utv/jdk1.3.1/jaxp-1.1/docs/api/java/lang/String.html")
;;(browse-url  "D:/Utv/jdk1.3.1/docs/api/java/lang/String.html")

;; Bind key to main tag lookup function
;;(global-set-key [(meta ,)] 'jtags-find-tag)
;;(global-set-key [(control ,)] 'jtags-member-completion)
(add-hook
 'java-mode-hook
 (function
  (lambda ()
    (define-key java-mode-map [(meta ,)] 'jtags-find-tag)            
    (define-key java-mode-map [(control ,)] 'jtags-member-completion))))

;; This doesn't work, use customize instead.
;;!(add-hook
;;! 'jde-mode-hook
;;! (function
;;!  (lambda ()
;;!    (define-key jde-mode-map [(meta ,)] 'jtags-find-tag)            
;;!    (define-key jde-mode-map [(control ,)] 'jtags-member-completion))))


;; ----------------------------------------------------------------------------
;; Generic functions:
;; ----------------------------------------------------------------------------

(defun list-uniqify (src)
  "Returns a copy of list SRC with all duplicates removed. The original list 
is not modified.

Example:

\(list-uniqify '\(1 3 1 5 3 1\)\) -> \(1 3 5\)"
  (list-uniqify-iter src nil))

(defun list-uniqify-iter (src unique)
  "Subfunction used by `list-uniqify'."
  (if (null src)
      unique
    (if (list-contains unique (car src))
	(list-uniqify-iter (cdr src) unique)
      (list-uniqify-iter (cdr src) (append unique (list (car src)))))))

;; (list-uniqify '("A" "B" "A" "C" "C" "A" "A" "D"))
;; (list-uniqify '("A" "B"))
;; (list-uniqify '("A" "A"))
;; (list-uniqify '("A"))
;; (list-uniqify '())

(defun list-contains (src item)
  "Returns true if ITEM can be found in list SRC, otherwise nil.

Example:

\(list-contains '\(1 2 3 4 5\) 3\) -> t
\(list-contains '\(1 2 3 4 5\) 7\) -> nil"
  (if (null src)
      nil
    (if (equal (car src) item)
	t
      (list-contains (cdr src) item))))

;; (list-contains '("1" "2" "3" "4" "5") "1")
;; (list-contains '("1" "2" "3" "4" "5") "5")
;; (list-contains '("1" "2" "3" "4" "5") "7")

;; use cl-seq routines instead??
;; (find '(lambda (x) (string-match "1" (car x))) '("1" "2" "3" "4" "5"))
;; (find "5" '("1" "2" "3" "4" "5"))
;; (find "7" '("1" "2" "3" "4" "5"))

(defun point-to-line (pos)
  "Converts a point (position in the current buffer) to a line number."
  (save-excursion
    (goto-char pos)
    (get-line)))

(defun get-line ()
  "Returns the line number of the current buffer position."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 (point)))))

;; ----------------------------------------------------------------------------
;; Functions for finding which class(es) the point is in:
;; ----------------------------------------------------------------------------

(defun jtags-get-surrounding-classes (filename line)
  "Returns a list of classes that the point is within, with the innermost 
class first and outermost class last. FILENAME is the name of the current 
buffer, and LINE is the line the point is on."
  (jtags-filter-class-list (find-classes filename) line))

(defun jtags-filter-class-list (class-list line)
  "Subfunction used by `jtags-get-surrounding-classes'.

Looks for the end of each class in the class list and decides if the point
is within that class or not. Returns a list that contains the names of the 
classes that the point is in, with the innermost class first and the 
outermost class last."
  (if (null class-list)
      nil
    (let* ((class-data (car class-list))
	   (class (car class-data))
	   (start-line (cdr class-data)))
      ;; (jtags-message "in jtags-filter-class-list: Class = %S" class-data)
      (goto-line start-line)
      (re-search-forward "{")
      (let* ((end-pos (scan-sexps (1- (point)) 1))
	     (end-line (point-to-line end-pos)))
	;; (jtags-message "in jtags-filter-class-list: Start: %d End: %d Line: %d" start-line end-line line)
	(if (and (>= line start-line) (<= line end-line))
	    (cons class (jtags-filter-class-list (cdr class-list) line))
	  (jtags-filter-class-list (cdr class-list) line))))))

;; ----------------------------------------------------------------------------
;; Functions for finding base classes for a list of classes:
;; ----------------------------------------------------------------------------

(defun jtags-get-class-list (&optional class)
  "Returns a list of classes that starts with the class that the point is in,
continues with its base classes and ends with Object. If the class that the 
point is in is an inner class, the list continues with the outer class and its
base classes.

If CLASS is specified, the returned class list will contain the specified 
class and its base classes. Otherwise, it will contain the class \(classes\) 
the point is in and its \(their\) base classes."
  (let ((buffer (current-buffer))
	(pos (point))
	(src (if (null class) 
		 (jtags-get-surrounding-classes (buffer-file-name) (get-line))
	       (list class))))
    ;; (jtags-message "in jtags-get-class-list: Classes = %S" src)
    (let ((res (list-uniqify (jtags-get-class-list-iter src))))
      (switch-to-buffer buffer)
      (goto-char pos)
      res)))

(defun jtags-get-class-list-iter (class-list)
  "Subfunction used by `jtags-get-class-list'."
  (if (null class-list)
      nil
    (append (jtags-do-get-class-list (car class-list))
	    (jtags-get-class-list-iter (cdr class-list)))))

;; (jtags-get-class-list-iter '("Foo"))
;; (jtags-get-class-list-iter '("Foo" "Bar" "Axe"))

(defun jtags-do-get-class-list (class)
  "Subfunction used by `jtags-get-class-list-iter'."
  ;; Lookup the class and if found, load its Java file
  ;; Otherwise, end the class list with this class
  (let ((definition (tags-lookup-method class)))
    (jtags-message "in jtags-do-get-class-list: Definition= %S" definition)
    (if (null definition)
	(list class)
      (find-file (car definition))
      (goto-line 1)
	  
      ;; Find out which class this class extends
      (let* ((rexp (concat "class\\W+" class "\\W+extends\\W+"))
	     (start-pos (re-search-forward rexp nil t))
	     (end-pos (re-search-forward "\\W" nil t)))
	;; (jtags-message "in jtags-do-get-class-list: Start: %S End: %S" start-pos end-pos)
		
	;; If it does not extend any class, end the class list
	;; Otherwise, find out which class the base class extends
	(if (null start-pos)
	    (list class "Object")
	  (cons class (jtags-do-get-class-list (buffer-substring-no-properties 
					  start-pos (1- end-pos)))))))))

;; (jtags-do-get-class-list "Axe")
;; (jtags-do-get-class-list "Bar")
;; (jtags-do-get-class-list "Foo")
;; (jtags-do-get-class-list "String")

;; ----------------------------------------------------------------------------
;; Functions for looking up tags:
;; ----------------------------------------------------------------------------

;; Looks up the identifiers in the list of classes. The first identifier in 
;; the list is searched for in the list of classes. The second identifier in 
;; the list is searched for in the class returned from the first identifier
;; and so on until the last identifier in the list (the one that we actually 
;; are interested in) has been looked up. Then a list containing the file 
;; name, a position in the file and the Type is returned.
;;
;; @param identifiers The list of identifiers to lookup.
;; @param classes The list of classes to lookup the first identifier in.
;; @return A list containing the file name and the line in the file where the 
;;         identifier is defined or nil if the definition could not be found.
;;
;; TODO: - If tags-lookup-method returns a simple type, e.g. "int" or "void"
;;         we should return nil.
;;       - handle this, super  (in jtags-local-tags-lookup?)

(defun jtags-recursive-tags-lookup (identifiers classes)
  "Looks up a tag recursively."
  ;; (jtags-message "in jtags-recursive-tags-lookup: Identifiers: %S\nClasses: %S" identifiers classes)
  (if (null identifiers)
      nil
    (let ((res nil))
      ;; If no classes to look in, lookup identifier as a class. 
      ;; Otherwise, look it up as a method/attribute in the first class.
      (if (null classes)
	  (setq res (tags-lookup-method (car identifiers)))
	(setq res (tags-lookup-method (car classes) (car identifiers))))
      (jtags-message "in jtags-recursive-tags-lookup: Result: %S" res)
	  
      ;; If we did not find anything, look in next class if there is one
      (if (null res)
	  (if (and (not (null classes)) (not (null identifiers)))
	      (jtags-recursive-tags-lookup identifiers (cdr classes)))
	
	;; If this is the last identifier in the list, return the result.
	;; Otherwise, lookup the next identifier using the class (and its
	;; base classes) returned from this lookup.
	(if (= (length identifiers) 1)
	    (list (car res) (caddr res) (cadr res))
	  (jtags-recursive-tags-lookup (cdr identifiers) 
				    (jtags-get-class-list (cadr res))))))))  ;; TODO consider multiple methods with same name but different type

(defun jtags-local-tags-lookup (identifiers class-list)
  "Looks up a local variable definition."
  ;; Use start of class as search boundary
  (let* ((boundary (nth 3 (tags-lookup-method (car class-list))))
	 (local (find-declaration (car identifiers) boundary)))
    ;;(jtags-message "in jtags-local-tags-lookup: Local definition: %s" local)
    (if local
	;; If there are more identifiers, look them up.
	;; Otherwise, return the filename and line of the local variable.
	(if (cdr identifiers)
	    (jtags-recursive-tags-lookup (cdr identifiers) 
				      (jtags-get-class-list (car local)))
	  (list (buffer-file-name) (cadr local) (car identifiers))))))

;; ----------------------------------------------------------------------------
;; Functions for looking up completions:
;; ----------------------------------------------------------------------------

(defun jtags-find-member-completions ()
  "Looks up the identifier in the marked expression or where the point is 
positioned and returns the text to be completed and the completable members
of the class, nil otherwise"
  (interactive)
  (save-excursion
    (let* (
	   (identifiers (reverse (parse-java-line)))
	   (to-be-completed (car identifiers))
	   (classes (jtags-get-class-list))
	   definition
	   members
	   orig_point)

      (setq orig_point (point))
      ;; Is there some text to be completed? 
      (if (looking-at "\[A-Za-z0-9_\]")
	  (setq identifiers (reverse (cdr identifiers)))
	(skip-chars-backward " \t\n")
	(backward-char)
		
	(if (not (eq (char-after) ?\.));; are we at a .
	    (setq identifiers (reverse (cdr identifiers)))
	  ;; we will complete the empty string
	  (setq identifiers (reverse identifiers))
	  (setq to-be-completed "")
	  ))
      (goto-char orig_point)
      
      (jtags-message "in jtags-find-member-completions: identifiers = %s and to-be-completed=%s" identifiers to-be-completed)
      
      (setq definition (jtags-recursive-tags-lookup identifiers classes))
      ;; If we did not find anything, check if first identifier is a local 
      ;; variable
      (unless definition
	(setq definition (jtags-local-tags-lookup identifiers classes)))
      (when definition
	(setq members (nth 5 (tags-lookup-method (caddr definition))))
	;;(jtags-message "in jtags-find-member-completions: %s has members %s" (caddr definition) members)
	;; If class had members
	(when members
	  ;; find matching members
	  (setq members
		(delete-if
		 (lambda (x)
		   (if (string-match to-be-completed x)
		       nil
		     x))
		 members))
	  ;; get rid of duplicates
	  (setq members (list-uniqify members))
	  ;; return sorted list
	  (cons to-be-completed
		(sort members              ;; sort items that begins with the text to be
		      '(lambda (a1 b1)     ;; replaced first. Scary code.
			 (let ((a-begins-with-text-to-be-completed (eq 0 (string-match to-be-completed a1)))
			       (b-begins-with-text-to-be-completed (eq 0 (string-match to-be-completed b1))))
			   (or 
			    (and
			     a-begins-with-text-to-be-completed
			     (not b-begins-with-text-to-be-completed))
			    (and a-begins-with-text-to-be-completed
				 (string< a1 b1))
			    (and (not b-begins-with-text-to-be-completed)
				 (string< a1 b1))))
			 ))))))))


;; ----------------------------------------------------------------------------
;; Functions that operate on the tagfile
;; ----------------------------------------------------------------------------
  
(defun tags-lookup-method (class &optional method package)
  "Lookup method in class in tagfile returns  the following list
   list'(filename      ;; Name of the source file where method is defined is defined
         type          ;; The class where method is defined.
         fileline)     ;; The line nr where method is defined
 upon success, nil otherwise.

If no method is supplided class is searched and the following list returned

   list'(filename      ;; Name of the source file where class is defined
         type          ;; What Class, class is, supplied for compability.
         fileline      ;; The line nr where class is defined
         filepos       ;; The file position where class is defined
         first-method-pos ;; The file position of the first method. This can be used 
                          ;; if only part of the file needs to be opened in order to 
                          ;; search for extends, implements etc.
         member-list   ;; A list of all members, used for completion

If package is provided, a check will be made with filename"

  (save-excursion
    (if (null tags-table-list)
	(visit-tags-table-buffer);;type file-name line file-pos tags-file-name))
      ;;(setq p tags-table-list)
      (let* ((p tags-table-list) list)
	(if running-xemacs
	    (setq p (buffer-tag-table-list)))
	(while (stringp (car p))
	  (if running-xemacs
	      (set-buffer (get-tag-table-buffer (car p)))
	    (visit-tags-table-buffer (car p)))
	  ;; Set case sensitive
	  (setq case-fold-search nil)
	  ;;(jtags-message "in tags-lookup-method: inside %s, remaining tags in %S" (buffer-name) p)
	  (let (class-pos (next-tag-pos nil) first-method-pos file-pos line type orig_pos file-name (right-package t) member-list)
	    (goto-char 1)
	    ;; find class
	    (while (and (re-search-forward (concat "class " class "\\W") nil t)
			(not list))
	      (setq class-pos  (point))
	      ;; find next tag to limit searching
	      (if (re-search-forward "\\(\\|\nclass\\)" nil t)
		  (setq next-tag-pos (point))
		(setq next-tag-pos nil))
	      
	      ;; look for all other members, methods and attributes.
	      (goto-char class-pos)
	      (while (re-search-forward "\n[^\n]*\\(\\<\[A-Za-z0-9_\]+\\) ?(?" next-tag-pos t)  ;; TODO \\< matches _ which is unfortunate.
		(setq member-list (cons  (buffer-substring-no-properties (match-beginning 1) (match-end 1)) member-list)))
	      ;; go back to class 
	      (goto-char class-pos)
	      
	      ;;(jtags-message "in tags-lookup-method: class-pos, next-tag-pos %d, %S" class-pos next-tag-pos)
	      (if method
		  (progn
		    ;; look for method TODO make case sensitive search
		    (when (re-search-forward (concat "\\<" method "[( ]") next-tag-pos t)
					  
		      (jtags-message "in tags-lookup-method: Found method=%s, line=%s at %s" method (buffer-substring-no-properties (match-beginning 0) (match-end 0)) (get-line))
		      ;; find type
		      (backward-char 3)
		      (search-backward " ")
		      (setq orig_pos (point))
		      (search-backward " ")
		      (forward-char)
		      (setq type   (buffer-substring-no-properties orig_pos (point)))
		      ))
		;; we were looking for a class.
		(setq type class)
		)
	      ;; If we have a match
	      (when type
		;; save file position/line
		(setq file-pos (return-tagged-pos))
		(setq line (return-tagged-line))
		(end-of-line)

		(when (null method)
		  ;; Lookup first method, of there is any
		  (when (re-search-forward "" next-tag-pos t)
		    (end-of-line)
		    (setq orig_pos (point))
		    (re-search-backward "\\<")
		    (setq first-method-pos (string-to-number (buffer-substring orig_pos (point))))))
				
		;; save file name - TODO make call to file-of-tag in etags.el GNU emacs and Xemacs.
		(search-backward "")
		(next-line 1)
		(beginning-of-line)
		(setq orig_pos (point))
		(end-of-line)
		(search-backward ",")
		(setq file-name (buffer-substring orig_pos (point))) 
		(jtags-message "in tags-lookup-method: *Status tagfile=%s type=%s filepos=%d file=%s " (buffer-name) type file-pos file-name)
		
		;; check if package matches file path
		(when package
		  (setq right-package (string-match (concat-with-fileseparator (split-string package "\\\.")) file-name))
		  (jtags-message "in tags-lookup-method: right-package? %s    link=%s file=%s" right-package
				 (concat-with-fileseparator (split-string package "\\\.")) file-name)
		  )
		(if (not right-package)
		    ;; put cursor at next class. 
		    (unless (search-forward "" nil t)
			(end-of-buffer))
		  ;; found the right class!
		  (setq p nil)
		  (if method
		      (setq list (list file-name type line))
		    (setq list (list file-name type line file-pos first-method-pos member-list))
		    (jtags-message "in tags-lookup-method: returna the list %s " list)
		    list
		    )))))
	  ;; try look in next tag file
	  (when p
		(setq p (cdr p))
		(jtags-message "trying next tagfile = %s" p))
	  )
	(if list
	    list
	  nil)
	))))  ;;;tags-lookup-method

;; (tags-lookup-method "Bar")
;; (tags-lookup-method "Bar_Inn" "foo")            ;; false
;; (tags-lookup-method "Bar" "foo")
;; (tags-lookup-method "Foo")
;; (tags-lookup-method "Foo" nil "pkg.hupp")   

(defun return-tagged-pos ()
  "Help function to tags-lookup-method, parses current row in tagfile and returns pos"
  (let (save-pos orig_pos pos)
  ;; save file positions
    (setq save-pos (point))
    (end-of-line)
    (setq orig_pos (point))
    (re-search-backward "\\<")
    (setq pos (string-to-number (buffer-substring orig_pos (point))))
    (goto-char save-pos)
    pos
    ))
;;(return-tagged-pos)   ;; 	public void print(Test.print11,122

(defun return-tagged-line()
  "Help function to tags-lookup-method, parses current row in tagfile and returns line"
  (let (save-pos orig_pos  line)
    ;; save line nr
    (setq save-pos (point))
    (end-of-line)
    (re-search-backward "")
    (re-search-forward "\\<")
    (setq orig_pos (point))
    (re-search-forward "\\>")
    (setq line (string-to-number (buffer-substring orig_pos (point))))
    (goto-char save-pos)
    line
    ))
;;(return-tagged-line)   ;; 	public void print(Test.print11,122

(defun concat-with-fileseparator (list)
  "Takes a list of directories and returns a string with
file separator regex [\\/] in between"
  (if (cdr list)
      (concat (car list) "[\\/]" (concat-with-fileseparator (cdr list)))
    (car list)))

;;; (string-match  (concat-with-fileseparator (split-string "java.lang.String" "\\\.")) file-name)

;;; This would have been more general
;;
;;(defun concat-with-separator (list separator)
;;  "Takes a list of directories and returns a string with
;;separator (ex: regex [\\/]) in between"
;;  (if (cdr list)
;;	(concat (car list) separator (concat-with-separator (cdr list) separator))
;;    (car list)))


(defun find-classes (file)
  "find classes in file and return list of (classes . file line) in the given file"
  (save-excursion
    (if (null tags-table-list)
	(visit-tags-table-buffer)
      ;;(setq p tags-table-list)
      (let* ((p tags-table-list) (list nil) orig_point boundary (continue-search t) class file-pos)
	;; for each tag table buffer;;
	(while (stringp (car p))
	  (if running-xemacs
	      (set-buffer (get-tag-table-buffer (car p)))
	    (visit-tags-table-buffer (car p)))
	  ;; Set case insensitive
	  (setq case-fold-search t)
	  ;; look for file in tags
	  (goto-char 1)
	  (when (search-forward file nil t)
	    ;; limit search, set boundary
	    (setq orig_point (point))
	    (if (search-forward "" nil t)
		(setq boundary (point))
	      (setq boundary nil))
	    (goto-char orig_point)
	    ;; lookup classes
	    (while continue-search
	      (setq continue-search (re-search-forward "class \\(\[A-Za-z0-9_\]\\)+\\W" boundary t))
	      (jtags-message "in find-classes: line %s charafter='%s'  found class=%s  list of classes=%s" (get-line) (char-to-string (char-after)) continue-search list)
	      (when continue-search
		;; save class name
		(setq class (buffer-substring-no-properties
			     (+ (match-beginning 0) 6) (1- (point))))
		;; get file line nr
		(end-of-line)
		(search-backward ",")
		(setq orig_pos (point))
		(re-search-backward "\\<")
		(setq file-pos (string-to-number (buffer-substring-no-properties orig_pos (point))))
		(setq list (cons (cons class file-pos) list))
		)))
	  (setq p (cdr p)))
	list))))

;; ----------------------------------------------------------------------------
;; Functions that parses javafiles
;; ----------------------------------------------------------------------------

;; Returns a list of identifiers that starts with the first identifier in the 
;; expression and ends with the last identifier, i.e. the one that we want to
;; lookup. Example: The expression toString().substring(1, 2).length(); returns
;; the list ("toString" "substring" "length"). Note that some of the 
;; identifiers might be attributes and not methods and that the first might 
;; actually be a class name, e.g. Class.forName("Foo").
;;
;; Basic test: parse-java-line
;; aa_a .	bb_b()  .   cc_c(pp1, qq2).ddd.ee_e()
;; "hej " . toString;
(defun parse-java-line ()
  "Parse java line at point and return a list of call order"
  (interactive)
  (save-excursion
    (let (bol (list nil) (match nil) orig_pos start_boundary)

      ;; set boundary
      (save-excursion
	(if (re-search-backward "\[;{\]" nil t)
	    (setq start_boundary (point))
	  (setq start_boundary 2))
	)

      (unless (looking-at "\[A-Za-z0-9_\]")
	(re-search-backward "\[A-Za-z0-9_\]" nil t)
	)

      (while (< start_boundary (point)) 
	;; find end of first match
	(re-search-forward "\[^A-Za-z0-9_\]" nil t)
	(goto-char (match-beginning 0))
	(setq orig_pos (point))
	;; Normally the search goes beyond point, but not if EoF 
	(if (eq (1+ (point)) (point-max))
	      (setq orig_pos (1+ (point))))
	(jtags-message "in parse-java-line: Am I looking at a char? %s  what is end char=%s" (looking-at "\[A-Za-z0-9_\]") (char-to-string (char-after)))
	;;(jtags-message "in parse-java-line: I am at %s char '%s'" (buffer-substring bol (point)) (char-to-string (char-after)))

	;; find start
	(re-search-backward "\[^A-Za-z0-9_\]" start_boundary t)
	(goto-char (match-beginning 0))
	(forward-char)
	(jtags-message "in parse-java-line: Am I looking at a char? %s  what is start char=%s" (looking-at "\[A-Za-z0-9_\]") (char-to-string (char-after)))
	(setq match (buffer-substring orig_pos (point)))
	  
	;; add keyword to list
	(setq list (cons (buffer-substring-no-properties orig_pos (point)) list))
	(jtags-message "in parse-java-line: latest match '%s' and list %s" match list)

	;; skip spaces
	(jtags-message "in parse-java-line: looking at a char? %s  char='%s' looking for new ."
		 (looking-at "\[A-Za-z0-9_\]") (char-to-string (char-after)))
	(skip-chars-backward " \t\n")
	(backward-char)
		
	;;(jtags-message "in parse-java-line: Is this a . at '%s' char '%s' %s"  (point) (char-to-string (char-after)) (eq (char-after) ?\.))
	;; look for '.'
	(if (not (eq (char-after) ?\.));; are we at a .
	    (setq start_boundary (point) + 1);; stop looking
	  ;; else find next keyword
	  (jtags-message "in parse-java-line: found point")
	  ;;(backward-char)
	  ;;(jtags-message "in parse-java-line: 9 I am at '%s' char '%s'" (buffer-substring bol (point)) (char-to-string (char-after)))
	  ;; skip spaces, tabs and newlines
	  (skip-chars-backward " \t\n")
	  (backward-char)

	  ;; constant String?
	  (when (eq (char-after) ?\")
	    (setq list (cons "String" list))
	    ;; stop searching
	    (setq start_boundary (1+ (point))))

	  ;; parenthesis?
	  ;;(jtags-message "in parse-java-line: 3 I am at '%s' char '%s'" (buffer-substring bol (point)) (char-to-string (char-after)))
	  (when (eq (char-after) ?\))
	    (goto-char (scan-sexps (1+ (point)) -1))
	    (backward-char)
	    ;; skip spaces
	    (skip-chars-backward " \t\n")
	    (backward-char)
	    )
	  ;;(jtags-message "in parse-java-line: 4 I am at '%s' char '%s'" (buffer-substring bol (point)) (char-to-string (char-after)))
	  ))
      (jtags-message "in parse-java-line: returns list = %s" list)
      list)));;//


(defun find-declaration (var &optional bound)
  "Looks backwards for a declaration, returns (type linenr point)"
  ;; look for declaration 
  (save-excursion
    ;; (jtags-message "in find-declaration: finding local declaration %s" var)
    (let (orig_pos var_pos (llist nil))
      ;; start at end of current word.
      (re-search-forward "[^A-Za-z0-9_]" nil t)
      (while (and (null llist)
		  (re-search-backward
		   (concat "[A-Za-z0-9_]+[ \t\n]+\\([A-Za-z0-9_]+[ \t\n]*,[ \t\n]*\\)*\\(" var "\\)[ \t\n]*\\(,[ \t\n]*[A-Za-z0-9_]+[ \t\n]*\\)*[=;\)]") bound t))
	(jtags-message "in find-declaration: find-declaration: properties at point=%s" (format " %s" (text-properties-at (point))))
	(setq var_pos (match-beginning 2))
	(if (string-match "font-lock-comment" (format " %s" (text-properties-at (point))));; TODO at string font lock check
	    nil
	  (jtags-message "in find-declaration: find-declaration: Found declaration at %s " (get-line))
	  (re-search-backward "\\\<")
	  (setq orig_pos (point))
		  
	  (re-search-forward  "[^A-Za-z0-9_]")
	  (backward-char 1)
	  (setq llist (list (buffer-substring-no-properties orig_pos (point)) (get-line) var_pos))
	  ))
      llist
      )))

;;  uncomment or turn off font-lock for the following lines to test find-declaration
;; BarbarClass ff, gg, hh ;
;; class ff;
;; function(Type rr)
;; funccall(ss, tt)

;;(find-declaration "ff" 9000)
;;(find-declaration "gg" 9000)
;;(find-declaration "hh" 9000)
;;(find-declaration "hh")
;;(find-declaration "rr")
;;(find-declaration "ss")    ;; shall FAIL
;;(find-declaration "tt")    ;; shall FAIL

;; basic test of find packages - do not edit this line
"
package acountry.acompany.aproduct.utilities.config;

import java.util.HashMap;

import acountry.acompany.aproduct.event.EventHandler;
import acountry.acompany.aproduct.server.base.MFConnectionMgr   ;
import acountry.acompany.aproduct.server.Server .*;
import acountry.acompany.aproduct.utilities.connection.  *  ;
"

(defun find-packages (kind &optional bound)
  "Looks for import or package, returns string list of packages"
  (save-excursion
    (let (list orig_pos end_pos)
      (goto-char 1)
      ;; debugging and basic test
      (if (equal (buffer-name) "tags-lookup.el")
	  (search-forward "basic test of find packages"))
      (while (re-search-forward (concat "\n" kind "[ \t\n]+\\\([^*\n;]*\\\)[ \t\n*]*;") bound t)
	(setq orig_pos (match-beginning 1))
	(goto-char (match-end 0))
	(re-search-backward "[a-zA-Z0-9_]" orig_pos t)
	(setq end_pos (match-end 0))
	(jtags-message "in find-packages: line=%s,  substring='%s' " (get-line) (buffer-substring-no-properties orig_pos end_pos))
	(setq list (cons (buffer-substring-no-properties orig_pos end_pos) list))
	)
      list)))

;;(find-packages "import")
;;(find-packages "package")

;; ----------------------------------------------------------------------------
;; HTML functions:
;; ----------------------------------------------------------------------------

;; ;; put correct path in frameset here 
;; (if (file-exists-p jtags-javadoc-frameset)
    
(defun jtags-prepare-javadoc-frameset (javafile javadoc-root)
  "Edit a frameset HTML file (jtags-javadoc-frameset) to show the command you are interested in. "
  (save-excursion
    (when (and jtags-javadoc-frameset (file-writable-p jtags-javadoc-frameset))
      ;; init if frameset does not exist
      (unless (file-exists-p jtags-javadoc-frameset)
	(jtags-init-javadoc-frameset javafile javadoc-root))

      ;; check if it is the right file (the right javadoc-root)
      (find-file jtags-javadoc-frameset)
      (goto-char 1)
      (unless (search-forward javadoc-root nil t)
	(kill-buffer (current-buffer))
	(jtags-init-javadoc-frameset javafile javadoc-root)
	(find-file jtags-javadoc-frameset)
	(goto-char 1))
      
      (search-forward "</FRAMESET>" nil t)
      (replace-regexp "src=\".+\" name"
		      (concat "src=\"" javafile "\" name"))
      (replace-regexp "HREF=\".+\">Non"
		      (concat "HREF=\"" javafile "\">Non"))
      (save-buffer)
      (kill-buffer (current-buffer))
      (jtags-message "in jtags-prepare-javadoc-frameset: susccesfully created %s" jtags-javadoc-frameset)
      )))

;; (jtags-prepare-javadoc-frameset "D:/utv/jdk1.3.1/docs/api/java/util/AbstractList.html" "D:/utv/jdk1.3.1/docs/api/")

(defun jtags-init-javadoc-frameset (javafile javadoc-root)
  "Prepares a javadoc frameset. Javadoc usually creates an index.html with these files:
overview-frame.html, allclasses-frame.html, overview-summary.html"
    (when (file-exists-p (concat javadoc-root "index.html"))
      (copy-file (concat javadoc-root "index.html") jtags-javadoc-frameset 'OK-IF-ALREADY-EXISTS)
      (find-file jtags-javadoc-frameset)
      (goto-char 1)
      ;; put javadoc-root in file, in case of multiple javadocs we might have to reinit frameset
      (insert (concat "<!-- javadoc-root = " javadoc-root  ". Generated by jtags -->\n"))
      ;; insert full path to frames
      (search-forward "<FRAME src=\"" nil t)
      (insert javadoc-root)
      (search-forward "<FRAME src=\"" nil t)
      (insert javadoc-root)
      ))

;;(jtags-init-javadoc-frameset "D:/utv/jdk1.3.1/docs/api/java/util/AbstractList.html" "D:/utv/jdk1.3.1/docs/api/")


;; ----------------------------------------------------------------------------
;; function test
;; ----------------------------------------------------------------------------

(defun jtags-function-test ()
  "Tests by looking for // test01 performing jtags-find-tags and look for OK01 on same row"
  (let ((tag-dir (file-name-directory jtags-file))
	(files '("Bar.java"
		 "pkg/hupp/Axe.java"
		 "pkg/hupp/Foo.java"
		 "pkg/hupp/Test.java"))
	testresult testnr testbuffer orig-tags-table-list test-tags-file
	(origbuffer (buffer-name))
	(search-start 1))
    ;; create temporary TAG file
    (store-substring tag-dir (string-match "lisp/$" tag-dir) "test")
    (setq test-tags-file (concat tag-dir "TESTTAG"))
    (copy-file (concat tag-dir "TAGS") test-tags-file 'OK-IF-ALREADY-EXISTS)
    (setq orig-tags-table-list (copy-sequence tags-table-list))
    ;; insert directory
    (find-file test-tags-file)
    (mapcar (lambda (file)
	      (goto-char 1)
	      (while (search-forward file nil t)
		(replace-match (concat tag-dir file) nil t)))
	    files)
    (save-buffer)
    (jtags-message "in jtags-function-test: saved buffer %s" test-tags-file)

    (setq tags-table-list (list test-tags-file))
    (save-excursion
      ;;      (find-file (car files))
      (mapcar (lambda (file)
		(set-buffer origbuffer)
		(find-file (concat tag-dir file))
		(setq testbuffer (buffer-name))
		(beginning-of-buffer)
		(while (re-search-forward "//.*test\\([0-9][0-9]\\)" nil t)
		  (setq search-start (point))
		  (setq testnr (match-string-no-properties 1))
		  (jtags-message "in jtags-function-test: found test nr %s" testnr)
		  (re-search-backward "//" nil t)
		  (backward-char 2)
		  (re-search-backward "\\<")
		  (if (null (jtags-find-tag))
		      (setq testresult (cons (concat testnr " failed") testresult))
		    (if (re-search-forward (concat "//.*OK" testnr) nil t)
			(setq testresult (cons (concat testnr " succeded") testresult))
		      (setq testresult (cons (concat testnr "failed") testresult)))
		    (jtags-message "in jtags-function-test: result = %s" (car testresult)))
		  (set-buffer testbuffer)
		  ;; make ready for next search
		  (goto-char search-start)
		  (jtags-message "in jtags-function-test: testbuffer line is %s, search-start is %s" (get-line) search-start)
		  (next-line 1)
		  (beginning-of-line)
		  ))
	      files))
    (set-buffer origbuffer)
    (when (> (length testbuffer) 1)
      (setq testresult (sort testresult 'string<))
      (mapcar (lambda (x)
		(message "TEST  -> %s" x)
		t)
	      testresult))
    ;; restore TAG file
    (setq tags-table-list (copy-sequence orig-tags-table-list))
    (switch-to-buffer "*Messages*")
    ))

;; Modifies a TESTTAG file and sets tags-table-list ti it. answer yes when running
;;(jtags-function-test)

(provide 'jtags)
;; ----------------------------------------------------------------------------

