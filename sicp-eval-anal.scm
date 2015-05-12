@(heading "sicp-eval-anal")

(module sicp-eval-anal
  @("The analyzing evaluator from section 4.1.7")
  (anal-eval*
   analyze
   analyze-self-evaluating
   analyze-quoted
   analyze-variable
   analyze-assignment
   analyze-definition
   analyze-if
   analyze-lambda
   analyze-sequence
   analyze-application
   execute-application)
  (import chicken scheme)
  (use sicp-eval)
  (include "sicp-eval-anal-core.scm")) 
