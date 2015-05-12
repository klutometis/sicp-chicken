@(heading "sicp-eval-lazy")

(module sicp-eval-lazy
  @("The lazy evaluator from section 4.2.2")
  (actual-value
   apply*
   delay-it
   driver-loop
   eval*
   eval-if
   evaluated-thunk?
   force-it
   input-prompt
   list-of-arg-values
   list-of-delayed-args
   output-prompt
   thunk-env
   thunk-exp
   thunk-value
   thunk?
   with-lazy-lists)
  (import chicken scheme)
  (use sicp-eval)
  (include "sicp-eval-lazy-core.scm")) 
