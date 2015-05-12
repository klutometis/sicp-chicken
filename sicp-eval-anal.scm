@(heading "sicp-eval-anal")

(module sicp-eval-anal
  @("The analyzing evaluator from section 4.1.7")
  (anal-eval*
   analyze
   analyze-application
   analyze-assignment
   analyze-definition
   analyze-if
   analyze-lambda
   analyze-quoted
   analyze-self-evaluating
   analyze-sequence
   analyze-variable
   execute-application)
  (import chicken scheme)
  (use sicp-eval)
  (include "sicp-eval-anal-core.scm")) 
