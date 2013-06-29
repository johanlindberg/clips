;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(defpackage :clips
  (:use :common-lisp)
  (:export :TRUE
	   :FALSE

	   :batch))
(in-package :clips)

(defconstant TRUE 'TRUE)
(defconstant FALSE 'FALSE)