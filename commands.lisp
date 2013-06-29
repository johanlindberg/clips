;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(in-package :clips)

(defun batch (filename)
  "Allows “batch” processing of CLIPS interactive commands by
   replacing standard input with the contents of a file.

   See CLIPS Basic Programming Guide '13.1.9 Executing Commands From
   a File' for more information.

   Syntax:
     (batch <file-name>)

   Doctests:
   >> (batch \"/Users/johanlindberg/Projects/cl-clips/batch-test.bat\")
   -> |TRUE
   42
   4
   4|
   TRUE
   "
  (if (load filename :print t)
      TRUE
      FALSE))
