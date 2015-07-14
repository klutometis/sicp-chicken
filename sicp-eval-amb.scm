@(heading "sicp-eval-amb")

(module sicp-eval-amb
  @("The non-deterministic backtracking evaluator from section 4.3.3")
  (amb-choices
   amb?
   ambeval*
   ambeval-fold
   ambeval-map
   ambeval-n
   analyze
   analyze-amb
   analyze-application
   analyze-assignment
   analyze-definition
   analyze-if
   analyze-lambda
   analyze-let
   analyze-quoted
   analyze-self-evaluating
   analyze-sequence
   analyze-variable
   driver-loop
   execute-application
   failure
   failure?
   get-args
   input-prompt
   let->combination
   let-body
   let-clause-expression
   let-clause-variable
   let-clause?
   let-clauses
   output-prompt
   success?
   with-natural-language
   with-require)
  (import chicken scheme)
  (use sicp-eval sicp-eval-anal)
  (include "sicp-eval-amb-core.scm"))
