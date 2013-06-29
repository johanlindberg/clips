;;;;              CL-CLIPS (a Common Lisp implementation of CLIPS)
;;;; ===========================================================================

(in-package :clips)

(defun batch (filename)
  "13.1.9 Executing Commands From a File

   Allows “batch” processing of CLIPS interactive commands by
   replacing standard input with the contents of a file. Any command
   or function can be used in a batch file, as well as construct
   definitions and responses to read or readline function calls. The
   load command should be used in batch files rather than defining
   constructs directly. The load command expects only constructs and
   hence moves to the next construct when an error occurs. The batch
   command, however, moves on until it finds the next construct or
   command (and in the case of a construct this is likely to generate
   more errors as the remaining commands and functions in the
   construct are parsed). This function returns TRUE if the batch
   file was successfully executed, otherwise FALSE is returned. Note
   that the batch command operates by replacing standard input rather
   than by immediately executing the commands found in the batch file.
   In effect, if you execute a batch command from the RHS of a rule,
   the commands in that batch file will not be processed until control
   is returned to the top‑level prompt.

   Syntax:
     (batch <file-name>)

   Doctests:
   >> (batch \"/Users/johanlindberg/Projects/cl-clips/batch-test.bat\")
   -> TRUE
   TRUE
   "
  (if (load filename :print t)
      TRUE
      FALSE))
