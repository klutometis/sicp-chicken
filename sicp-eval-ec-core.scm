(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register)
                 register-name))
      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (lambda (message)
      (case message
        ((get) contents)
        ((set) (lambda (value)
                 (set! contents value)))
        (else (error "Unknown request: REGISTER" message))))))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (lambda (message)
      (case message
        ((push) push)
        ((pop) (pop))
        ((initialize) (initialize))
        (else
         (error "Unknown request: STACK" message))))))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list
            (list 'initialize-stack
                  (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc)
                 (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons
                   (list name
                         (make-register name))
                   register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc
                  (car insts)))
                (execute)))))
      (lambda (message)
        (case message
          ((start)
           (set-contents! pc the-instruction-sequence)
           (execute))
          ((install-instruction-sequence)
           (lambda (seq)
             (set! the-instruction-sequence seq)))
          ((allocate-register) allocate-register)
          ((get-register) lookup-register)
          ((install-operations)
           (lambda (ops)
             (set! the-ops (append the-ops ops))))
          ((stack) stack)
          ((operations) the-ops)
          (else (error "Unknown request: MACHINE" message)))))))

(define (start machine) (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents
   (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                 (cons (make-label-entry next-inst insts)
                       labels))
               (receive (cons (make-instruction next-inst) insts)
                 labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
        (lambda (inst)
          (set-instruction-execution-proc!
           inst (make-execution-procedure
                 (instruction-text inst)
                 labels
                 machine
                 pc
                 flag
                 stack
                 ops)))
      insts)))

(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (case (car inst)
    ((assign) (make-assign inst machine labels ops pc))
    ((test) (make-test inst machine labels ops flag pc))
    ((branch) (make-branch inst machine labels flag pc))
    ((goto) (make-goto inst machine labels pc))
    ((save) (make-save inst machine stack pc))
    ((restore) (make-restore inst machine stack pc))
    ((perform) (make-perform inst machine labels ops pc))
    (else (error "Unknown instruction type: ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp
                machine
                labels
                operations)
               (make-primitive-exp
                (car value-exp)
                machine
                labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc) (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition
                machine
                labels
                operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction) (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE" symbol))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs (map (lambda (e) (make-primitive-exp e machine labels))
                     (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops) (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define eval-dispatch
  '(eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))))

(define ev-self-eval
  '(ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))))

(define ev-variable
  '(ev-variable
    (assign val
            (op lookup-variable-value)
            (reg exp)
            (reg env))
    (goto (reg continue))))

(define ev-quoted
  '(ev-quoted
    (assign val
            (op text-of-quotation)
            (reg exp))
    (goto (reg continue))))

(define ev-lambda
  '(ev-lambda
    (assign unev
            (op lambda-parameters)
            (reg exp))
    (assign exp
            (op lambda-body)
            (reg exp))
    (assign val
            (op make-procedure)
            (reg unev)
            (reg exp)
            (reg env))
    (goto (reg continue))))

(define ev-application
  '(ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))))

(define ev-appl-did-operator
  '(ev-appl-did-operator
    (restore unev)
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)))

(define ev-appl-operand-loop
  '(ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))))

(define ev-appl-accumulate-arg
  '(ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))))

(define ev-appl-last-arg
  '(ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))))

(define ev-appl-accum-last-arg
  '(ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))))

(define apply-dispatch
  '(apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))))

(define primitive-apply
  '(primitive-apply
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (restore continue)
    (goto (reg continue))))

(define compound-apply
  '(compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
    (assign env (op procedure-body) (reg proc))
    (goto (label ev-sequence))))

(define ev-begin
  '(ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))))

(define ev-sequence
  '(ev-sequence
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-sequence-last-exp))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))))

(define ev-sequence-continue
  '(ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))))

(define ev-sequence-last-exp
  '(ev-sequence-last-exp
    (restore continue)
    (goto (label eval-dispatch))))

(define ev-sequence
  '(ev-sequence
    (test (op no-more-exps?) (reg unev))
    (branch (label ev-sequence-end))
    (assign exp (op first-exp) (reg unev))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))))

(define ev-sequence-continue
  '(ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))))

(define ev-sequence-end
  '(ev-sequence-end
    (restore continue)
    (goto (reg continue))))

(define ev-if
  '(ev-if
    (save exp)
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch))))

(define ev-if-decide
  '(ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))))

(define ev-if-alternative
  '(ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))))

(define ev-if-consequent
  '(ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))))

(define ev-assignment
  '(ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev)
    (assign exp (op assignment-variable) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch))))

(define ev-assignment-1
  '(ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op set-variable-value!)
             (reg unev)
             (reg val)
             (reg env))
    (assign val (const ok))
    (goto (reg continue))))

(define ev-definition
  '(ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev)
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch))))

(define ev-definition-1
  '(ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform (op define-variable!)
             (reg unev)
             (reg val)
             (reg env))
    (assign val (const ok))
    (goto (reg continue))))

(define read-eval-print-loop
  '(read-eval-print-loop
    (perform (op initialize-stack))
    (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
    (assign exp (op read))
    (assign env (op the-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))))

(define print-result
  '(print-result
    (perform (op announce-output)
             (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))))

(define unknown-expression-type
  '(unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))))

(define unknown-procedure-type
  '(unknown-procedure-type
    (restore continue)
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))))

;; (define signal-error
;;   '(signal-error
;;     (perform (op user-print) (reg val))
;;     (goto (label read-eval-print-loop))))

(define signal-error
  '(signal-error
    (perform (op debug) (reg val))
    (goto (label done))))

(define eceval-operations
  `((adjoin-arg ,adjoin-arg)
    (announce-output ,announce-output)
    (application? ,application?)
    (apply-primitive-procedure ,apply-primitive-procedure)
    (assignment-variable ,assignment-variable)
    (assignment? ,assignment?)
    (begin-actions ,begin-actions)
    (begin? ,begin?)
    (compound-procedure? ,compound-procedure?)
    (debug ,(lambda x (debug x)))
    (define-variable! ,define-variable!)
    (definition-value ,definition-value)
    (definition-variable ,definition-variable)
    (definition? ,definition?)
    (empty-arglist ,empty-arglist)
    (extend-environment ,extend-environment)
    (first-exp ,first-exp)
    (first-operand ,first-operand)
    (if-alternative ,if-alternative)
    (if-consequent ,if-consequent)
    (if-predicate ,if-predicate)
    (if? ,if?)
    (lambda-body ,lambda-body)
    (lambda-parameters ,lambda-parameters)
    (lambda? ,lambda?)
    (last-exp? ,last-exp?)
    (last-operand? ,last-operand?)
    (lookup-variable-value ,lookup-variable-value)
    (make-procedure ,make-procedure)
    (no-more-exps? ,no-more-exps?)
    (no-operands? ,no-operands?)
    (operands ,operands)
    (operator ,operator)
    (primitive-procedure? ,primitive-procedure?)
    (procedure-body ,procedure-body)
    (procedure-environment ,procedure-environment)
    (procedure-parameters ,procedure-parameters)
    (prompt-for-input ,prompt-for-input)
    (quoted? ,quoted?)
    (read ,read)
    (rest-exps ,rest-exps)
    (rest-operands ,rest-operands)
    (self-evaluating? ,self-evaluating?)
    (set-variable-value! ,set-variable-value!)
    (text-of-quotation ,text-of-quotation)
    (the-global-environment ,the-global-environment)
    (true? ,true?)
    (user-print ,user-print)
    (variable? ,variable?)))

(define ev-eval
  '(ev-eval
    (perform (op initialize-stack))
    (assign continue (label done))
    (goto (label eval-dispatch))))

(define done '(done))

(define controller
  (append
   ev-eval
   eval-dispatch
   ev-self-eval
   ev-variable
   ev-quoted
   ev-lambda
   ev-application
   ev-appl-did-operator
   ev-appl-operand-loop
   ev-appl-accumulate-arg
   ev-appl-last-arg
   ev-appl-accum-last-arg
   apply-dispatch
   primitive-apply
   compound-apply
   ev-begin
   ev-sequence
   ev-sequence-continue
   ev-sequence-last-exp
   ev-sequence
   ev-sequence-continue
   ev-sequence-end
   ev-if
   ev-if-decide
   ev-if-alternative
   ev-if-consequent
   ev-assignment
   ev-assignment-1
   ev-definition
   ev-definition-1
   unknown-expression-type
   unknown-procedure-type
   signal-error
   done))
