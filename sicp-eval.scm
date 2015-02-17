@(heading "sicp-eval")

(module sicp-eval
  @("Evaluation procedures from section 4.1")
  (application?
   apply*
   assignment?
   begin-actions
   begin?
   cond?
   cond->if
   cond-actions
   cond-clauses
   cond-else-clause?
   cond-predicate
   definition?
   driver-loop
   eval*
   eval-assignment
   eval-definition
   eval-if
   eval-sequence
   if?
   first-operand
   lambda?
   lambda-body
   lambda-parameters
   list-of-values
   lookup-variable-value
   make-if
   make-procedure
   no-operands?
   operands
   operator
   primitive-procedures
   quoted?
   rest-operands
   self-evaluating?
   sequence->exp
   setup-environment
   tagged-list?
   text-of-quotation
   the-global-environment
   variable?)
  (import chicken scheme)
  (use debug)
  (include "sicp-eval-core.scm"))
