;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(defpackage :clips
  (:use :common-lisp)
  (:shadow :assert)
  (:export :TRUE
	   :FALSE

	   ; Fact functions
	   :assert
	   :retract

	   ; Commands
	   :batch
	   :clear
	   :facts))
(in-package :clips)

(defconstant TRUE 'TRUE)
(defconstant FALSE 'FALSE)

(defvar *working-memory* (make-hash-table))
(defvar *fact-index* 0)

;;; Internal functions
;;; ----------------------------------------------------------------------------

(defmacro %make-implied-deftemplate (name multislot)
  "%make-implied-deftemplate makes a defstruct with one multislot (%multislot)"

  (let ((print-function (intern (format nil "%PRINT-~A" name))))
    `(progn
       (defun ,print-function (object stream d)
	 (declare (ignore object d))
	 (format stream "(~A~{ ~S~})" ',name ',multislot))
       (defstruct (,name (:print-function ,print-function)) %multislot)
       (,(intern (format nil "MAKE-~A" name)) :%multislot ',multislot))))

(defmacro %make-initial-fact ()
  `(progn
     (defun print-initial-fact (object stream d)
       (declare (ignore object d))
       (format stream "(INITIAL-FACT)"))
     (defstruct (initial-fact (:print-function print-initial-fact)))
     (make-initial-fact)))

(defmacro %insert-initial-fact ()
  `(progn
     (setf (gethash 0 *working-memory*)
	   (%make-initial-fact))
     (defparameter <FACT-0> (gethash 0 *working-memory*))))

;;; Fact functions
;;; ----------------------------------------------------------------------------

(defmacro assert (&rest rhs-patterns)
  "The assert action allows the user to add a fact to the fact‑list.

   Multiple facts may be asserted with each call. If the facts item is being
   watched  (see section 13.2), then an informational message will be printed
   each time a fact is asserted.

   See CLIPS Basic Programming Guide '12.9.1 Creating New Facts' for more
   information.

   Syntax:
     (assert <RHS-pattern>+)

     <RHS-pattern>      ::= <ordered-RHS-pattern> |
                            <template-RHS-pattern>

   Doctests:
   >> (progn
        (clips:clear)  ; Make sure to reset the working memory
        T)             ; before running these tests.
   T
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
		       ;; This defparameter is specific to this implementation.
		       ;; The symbol <FACT-n> does not point to a fact instance
		       ;; in CLIPS.
		       `((setf (gethash ,fact-index *working-memory*)
			       (%make-implied-deftemplate
				,(car rhs-pattern)
				,(cdr rhs-pattern)))
			 (defparameter ,(intern (format nil "<FACT-~D>" fact-index))
			   (gethash ,fact-index *working-memory*))))))
     rhs-patterns)

    `(progn
       ,@result
       ',(intern (format nil "<FACT-~D>" fact-index)))))

(defmacro retract (&rest retract-specifiers)
  "The retract action allows the user to remove facts from the fact‑list.

   Multiple facts may be retracted with a single retract statement. The
   retraction of a fact also removes all rules that depended upon that fact for
   activation from the agenda. Retraction of a fact may also cause the
   retraction of other facts which receive logical support from the retracted
   fact.

   If the facts item is being watched (see section 13.2), then an informational
   message will be printed each time a fact is retracted.

   The term <retract‑specifier> includes variables bound on the LHS to fact‑
   addresses as described in section 5.4.1.8, or the fact‑index of the desired
   fact (e.g. 3 for the fact labeled f‑3), or an expression which evaluates to a
   retract‑specifier. If the symbol * is used as an argument, all facts will be
   retracted. Note that the number generally is not known during the execution
   of a program, so facts usually are retracted by binding them on the LHS of a
   rule. Only variables, fact indices, or the symbol * may be used in a retract.
   External functions may not be called. This function has no return value.

   See CLIPS Basic Programming Guide '12.9.2 Removing Facts from the Fact‑list'
   for more information.

   Syntax:
     (retract <retract-specifier>+ | *)
 
     <retract-specifier> ::= <fact-specifier> | <integer-expression>

   Doctests:
   >> (progn
        (clips:clear)
        T)
   T
   >> (clips:assert (color red))
   <FACT-1>
   >> (progn
        (clips:retract 1)
        (hash-table-count *working-memory*))
   1 ; The only one left should be initial-fact
  "
  (mapcar #'(lambda (index)
	      (remhash index *working-memory*))
	  retract-specifiers)
  (values))

;;; Commands
;;; ----------------------------------------------------------------------------

(defun batch (filename)
  "Allows 'batch' processing of CLIPS interactive commands by
   replacing standard input with the contents of a file.

   See CLIPS Basic Programming Guide '13.1.9 Executing Commands From
   a File' for more information.

   Syntax:
     (batch <file-name>)

   Doctests:
   >> (batch \"/Users/johanlindberg/Projects/clips/batch-test.bat\")
   -> |TRUE
   42
   4
   4|
   TRUE
   "
  (if (load filename :print t)
      TRUE
      FALSE))

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

   See CLIPS Basic Programming Guide '13.1.6 Clearing CLIPS' for more
   information.

   Syntax:
     (clear)

   Doctests:
   >> (progn
        (clips:assert (color red))
        (clips:clear)
        (hash-table-count *working-memory*))
   1 ; This is the (initial-fact)
   >> (clips:assert (color red))
   <FACT-1>
   "
  (clrhash *working-memory*)
  (%insert-initial-fact)
  ;; Even though not explicitly stated in the docs the
  ;; clear command also resets the fact-index.
  (setf *fact-index* 0)
  (values))

(defun facts ()
  "Displays facts stored in the fact‑list.

   If <module‑name> is not specified, then only facts visible to the current
   module will be displayed. If <module‑name> is specified, then only facts
   visible to the specified module are displayed. If the symbol * is used for
   <module‑name>, then facts from any module may be displayed. If the start
   argument is specified, only facts with fact‑indices greater than or equal to
   this argument are displayed. If the end argument is specified, only facts
   with fact‑indices less than or equal to this argument are displayed. If the
   max argument is specified, then no facts will be displayed beyond the
   specified maximum number of facts to be displayed. This function has no
   return value.

   Syntax:
     (facts [<module-name>]
       [<start-integer-expression>
        [<end-integer-expression>
         [<max-integer-expression>]]])
 
   Doctests:
   >> (progn
        (clips:clear)
        (clips:assert (color red)) 
        (clips:facts)
        T)
   -> |f-0     (initial-fact)
       f-1     (color red)
       For a total of 2 facts.|
   T"
  (maphash #'(lambda (key value)
	       (format t "~&f-~A     ~S" key value))
	   *working-memory*)
  (format t "~&For a total of ~D facts." (hash-table-count *working-memory*))
  (values))

;;; Tests
;;; ----------------------------------------------------------------------------

(defun test-all ()
  (doctest:test #P"/Users/johanlindberg/Projects/clips/clips.lisp"))
