@(heading "sicp-eval-ec")

(module sicp-eval-ec
  @("The explicit control evaluator from chapter 5")
  (get-register-contents
   make-machine
   set-register-contents!
   start)
  (import chicken scheme)
  (use debug sicp-eval)
  (include "sicp-eval-ec-core.scm"))
