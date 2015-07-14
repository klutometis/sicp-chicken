(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  @("Evaluates the expression using backtracking search."
    (exp "The expression to evaluate")
    (env "The environment to evaluate it in")
    (succeed "The success-continuation")
    (fail "The failure-continuation")
    (@to "object"))
  ((analyze exp) env succeed fail))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value!
                             var old-value env)
                            (fail2)))))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (let-clause? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-clause-variable clause) (car clause))
(define (let-clause-expression clause) (cadr clause))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (let ((clauses (let-clauses exp)))
    (let ((variables (map let-clause-variable clauses))
          (expressions (map let-clause-expression clauses)))
      `((lambda ,variables ,@(let-body exp)) ,@expressions))))

(define (analyze-let exp)
  (let ((combination (let->combination exp)))
    (analyze combination)))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((let-clause? exp) (analyze-let exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))

;; Guaranteed to be unique.
(define failure (cons #f #f))
(define (failure? object) (eq? object failure))

(define (success? object) (not (eq? object failure)))

(define (ambeval-n exp env n)
  @("Amb-evaluates the expression, invoking the success-continuation {{n}} 
times or until failure."
    (exp "The expression to evaluate")
    (env "The environment to evaluate it in")
    (n "The maximum number of times to invoke the success continuation")
    (@to "object"))  
  (ambeval exp
           env
           (lambda (val next-alternative)
             (set! n (- n 1))
             (if (zero? n)
                 val
                 (next-alternative)))
           (lambda () failure)))

(define ambeval-fold
  @("Folds over the results of up to {{n}} successful executions of {{exp}};
if {{n}} is missing, folds over all successful executions
until failure."
    (exp "The expression to execute")
    (env "The environment to execute it in")
    (cons "The aggregator")
    (nil "The initial value")
    (n "The number of results to gather")
    (@to "list"))
  (case-lambda
   ((exp env cons nil) (ambeval-fold exp env cons nil +inf.0))
   ((exp env cons nil n)
    (let ((result nil))
      (ambeval exp
               env
               (lambda (val next-alternative)
                 (set! n (- n 1))
                 (if (negative? n)
                     result
                     (begin
                       (set! result (cons val result))
                       (next-alternative))))
               (lambda () result))))))

(define ambeval-map
  @("Maps over the results of up to {{n}} successful executions of {{exp}};
if {{n}} is missing, maps over all successful executions until failure."
    (exp "The expression to execute")
    (env "The environment to execute it in")
    (f "The function to apply to the results")
    (n "The number of results to gather")
    (@to "list"))
  (case-lambda
   ((exp env f) (ambeval-map exp env f +inf.0))
   ((exp env f n)
    (ambeval exp
             env
             (lambda (val next-alternative)
               (set! n (- n 1))
               (if (negative? n)
                   '()
                   (cons val (next-alternative))))
             (lambda () '())))))

(define ambeval*
  @("Gathers the results of up to {{n}} successful executions of {{exp}}; 
if {{n}} is missing, gathers all successful executions until failure."
    (exp "The expression to execute")
    (env "The environment to execute it in")
    (n "The number of results to gather")
    (@to "list"))
  (case-lambda
   ((exp env) (ambeval* exp env +inf.0))
   ((exp env n) (ambeval-map exp env values n))))

(define (with-require procedures receive-env)
  @("Installs {{require}}, {{an-element-of}}, {{an-integer-starting-from}} 
in the environment in addition to the primitive procedures enumerated 
in {{procedures}}; then calls {{receive-env}} with the configured environment."
    (procedures "A key-value list of primitive procedure-names and
their definitions")
    (receive-env "A lambda of one value that is called with the prepared
environment")
    (@to "object"))
  (with-primitive-procedures (append procedures `((member ,member)
                                                  (not ,not)))
    (lambda (env)
      ;; Interesting that we need an alternative for this if.
      (ambeval* '(define (require p) (if (not p) (amb) 'success)) env)
      (ambeval* '(define (an-element-of items)
                   (require (not (null? items)))
                   (amb (car items) (an-element-of (cdr items)))) env)
      (ambeval* '(define (an-integer-starting-from n)
                   (amb n (an-integer-starting-from (+ n 1)))) env)
      (ambeval*
       '(define (distinct? items)
          (cond ((null? items) true)
                ((null? (cdr items)) true)
                ((member (car items) (cdr items)) false)
                (else (distinct? (cdr items))))) env)
      (receive-env env))))

(define (with-natural-language procedures receive-env)
  @("Installs the natural-language primitives from 4.3.2."
    (procedures "A key-value list of primitive procedure-names and
their definitions")
    (receive-env "A lambda of one value that is called with the prepared
environment")
    (@to "object"))
  (with-require (append procedures
                        `((quote (lambda (x) (quote x)))
                          (list ,list)
                          (memq ,memq)))
  (lambda (env)
    (ambeval* '(define nouns
                 '(noun student professor cat class))
              env)
    (ambeval* '(define verbs
                 '(verb studies lectures eats sleeps))
              env)
    (ambeval* '(define articles
                 '(article the a))
              env)
    (ambeval* '(define prepositions
                 '(prep for to in by with))
              env)
    (ambeval* '(define (parse-sentence)
                 (list 'sentence
                       (parse-noun-phrase)
                       (parse-verb-phrase)))
              env)
    (ambeval* '(define (parse-prepositional-phrase)
                 (list 'prep-phrase
                       (parse-word prepositions)
                       (parse-noun-phrase)))
              env)
    (ambeval* '(define (parse-verb-phrase)
                 (define (maybe-extend verb-phrase)
                   (amb
                    verb-phrase
                    (maybe-extend
                     (list 'verb-phrase
                           verb-phrase
                           (parse-prepositional-phrase)))))
                 (maybe-extend (parse-word verbs)))
              env)
    (ambeval* '(define (parse-simple-noun-phrase)
                 (list 'simple-noun-phrase
                       (parse-word articles)
                       (parse-word nouns)))
              env)
    (ambeval* '(define (parse-noun-phrase)
                 (define (maybe-extend noun-phrase)
                   (amb
                    noun-phrase
                    (maybe-extend
                     (list 'noun-phrase
                           noun-phrase
                           (parse-prepositional-phrase)))))
                 (maybe-extend (parse-simple-noun-phrase)))
              env)
    (ambeval* '(define (parse-word word-list)
                 (require (not (null? *unparsed*)))
                 (require (memq (car *unparsed*)
                                (cdr word-list)))
                 (let ((found-word (car *unparsed*)))
                   (set! *unparsed* (cdr *unparsed*))
                   (list (car word-list) found-word)))
              env)
    (ambeval* '(define *unparsed* '()) env)
    (ambeval* '(define (parse input)
                 (set! *unparsed* input)
                 (let ((sent (parse-sentence)))
                   (require (null? *unparsed*))
                   sent))
              env)
    (receive-env env))))
