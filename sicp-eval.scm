@(heading "sicp-eval")

(module sicp-eval
  @("Evaluation procedures from section 4.1")
  (add-binding-to-frame!
   application?
   apply*
   apply-primitive-procedure
   assignment?
   assignment-value
   assignment-variable
   begin-actions
   begin?
   compound-procedure?
   cond?
   cond->if
   cond-actions
   cond-clauses
   cond-else-clause?
   cond-predicate
   define-variable!
   definition?
   definition-variable
   definition-value
   driver-loop
   enclosing-environment
   eval*
   eval-assignment
   eval-definition
   eval-if
   eval-sequence
   extend-environment
   false?
   first-frame
   frame-values
   frame-variables
   if?
   if-alternative
   if-consequent
   if-predicate
   first-operand
   lambda?
   lambda-body
   lambda-parameters
   list-of-values
   lookup-variable-value
   make-if
   make-frame
   make-procedure
   no-operands?
   operands
   operator
   primitive-procedure?
   primitive-procedures
   procedure-body
   procedure-environment
   procedure-parameters
   quoted?
   rest-operands
   self-evaluating?
   sequence->exp
   set-variable-value!
   setup-environment   
   tagged-list?
   text-of-quotation
   the-empty-environment
   the-global-environment
   true?
   variable?
   with-primitive-procedures)
  (import chicken scheme)
  (use debug)
  (include "sicp-eval-core.scm"))
