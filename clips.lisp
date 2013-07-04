;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(defpackage :clips
  (:use :common-lisp)
  (:shadow :assert)
  (:export :TRUE
	   :FALSE

	   ; Fact functions [12.9]
	   :assert
	   :retract

	   ; Commands
	   :batch
	   :clear))
(in-package :clips)

(defconstant TRUE 'TRUE)
(defconstant FALSE 'FALSE)

(defvar *working-memory* (make-hash-table))
(defvar *fact-index* 0)

(defmacro %make-implied-deftemplate (name multislot)
  "%make-implied-deftemplate makes a defstruct with one multislot (%multislot)"

  `(progn
     (defstruct ,name %multislot)
     (,(intern (format nil "MAKE-~A" name)) :%multislot ',multislot)))

(defun clear ()
  "Clears CLIPS.

   Removes all constructs  and all associated data structures (such as facts and
   instances) from the CLIPS environment. A clear may be performed safely at any
   time, however, certain constructs will not allow themselves to be deleted
   while they are in use. For example, while deffacts are being reset (by the
   reset command), it is not possible to remove them using the clear command.
   Note that the clear command does not effect many environment characteristics
   (such as the current conflict resolution strategy). This function has no
   return value.

   Syntax:
     (clear)

   Doctests:
   >> (progn
        (clips:assert (color red))
        (clips:clear)
        (hash-table-count *working-memory*))
   0
   "
  nil)


(defmacro assert (&rest rhs-patterns)
  "The assert action allows the user to add a fact to the fact‑list.

   Multiple facts may be asserted with each call. If the facts item is being
   watched  (see section 13.2), then an informational message will be printed
   each time a fact is asserted.

   Syntax:
     (assert <RHS-pattern>+)

   Doctests:

   >> (progn
        (clrhash *working-memory*) ; Make sure to reset the working memory
        (setf *fact-index* 0))     ; before running these tests.
   0
   >> (clips:assert (color red))
   <FACT-1>

   >> (clips:assert (color red) (color blue))
   <FACT-3>

   >> <FACT-3>
   #S(COLOR :%MULTISLOT (BLUE))
   >> <FACT-2>
   #S(COLOR :%MULTISLOT (RED))
   "
  (let ((result '())
	(fact-index -1))
    (mapcar
     #'(lambda (rhs-pattern)
	 (setf fact-index (incf *fact-index*))
	 (setf result
	       (append result 
		       `((defvar ,(intern (format nil "<FACT-~D>" fact-index))
			   (%make-implied-deftemplate ,(car rhs-pattern)
						      ,(cdr rhs-pattern)))
			 (setf (gethash ,fact-index *working-memory*)
			       ',(intern (format nil "<FACT-~D>" fact-index)))))))
     rhs-patterns)

    `(progn
       ,@result
       ',(intern (format nil "<FACT-~D>" fact-index)))))
