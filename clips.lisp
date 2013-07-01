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
	   :batch))
(in-package :clips)

(defconstant TRUE 'TRUE)
(defconstant FALSE 'FALSE)

(defvar *working-memory* (make-hash-table))
(defvar *fact-index* 0)

(defmacro %make-implied-deftemplate (name multislot)
  "%make-implied-deftemplate constructs a defstruct with one multislot (%multislot)"

  `(progn
     (defstruct ,name %multislot)
     (,(intern (concatenate 'string "MAKE-" (string name))) :%multislot ',multislot)))

(defmacro assert (&rest rhs-patterns)
  "The assert action allows the user to add a fact to the fact‑list.

   Multiple facts may be asserted with each call. If the facts item is being
   watched  (see section 13.2), then an informational message will be printed
   each time a fact is asserted.

   Syntax:
     (assert <RHS-pattern>+)

   Doctests:
   >> (clips:assert (color red))
   <Fact-0>
   "
  (let ((result '()))
    (mapcar
     #'(lambda (rhs-pattern)
	 (setf result
	       (append `(setf (gethash ,(incf *fact-index*) *working-memory*)
			      (%make-implied-deftemplate ,(car rhs-pattern)
							 ,(cdr rhs-pattern))))))
     rhs-patterns)
  result))
