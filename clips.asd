;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(defpackage :clips-system
  (:use :cl :asdf))
(in-package :clips-system)

(defsystem :clips
  :description "A CLIPS implementation in Common Lisp."
  :version "0.0.0 alpha"
  :author "Johan Lindberg (johan@pulp.se)"
  :licence "Public Domain"
  :components ((:file "clips")))
