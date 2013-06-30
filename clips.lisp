;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(defpackage :clips
  (:use :common-lisp)
  (:export :TRUE
	   :FALSE

	   ; Fact functions [12.9]
	   :assert
	   :retract

	   ; Commands
	   :batch)
  (:shadow :assert))
(in-package :clips)

(defconstant TRUE 'TRUE)
(defconstant FALSE 'FALSE)