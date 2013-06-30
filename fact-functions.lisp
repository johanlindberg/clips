

(defun assert (&rest rhs-patterns)
  "The assert action allows the user to add a fact to the fact‑list.

   Multiple facts may be asserted with each call. If the facts item is being
   watched  (see section 13.2), then an informational message will be printed
   each time a fact is asserted.

   Syntax:
     (assert <RHS-pattern>+)

   Doctests:
   >> (assert (color red))
   <Fact-0>
   "
  FALSE)
 