@(heading "sicp-constraints")

(module sicp-constraints
  @("Constraint satisfaction from section 3.3.5")
  (adder
   connect
   constant
   for-each-except
   forget-value!
   get-value
   has-value?
   set-value!
   inform-about-no-value
   inform-about-value
   make-connector
   multiplier
   probe)

  (import chicken extras scheme)

  (include "sicp-constraints-core.scm"))
