(define (has-value? connector)
  @("Does this connector have a value?"
    (connector "The connector to test")
    (@to "boolean"))
  (connector 'has-value?))

(define (get-value connector)
  @("Gets this connector's value."
    (connector "The connector to test")
    (@to "object"))
  (connector 'value))

(define (set-value! connector new-value informant)
  @("Sets this connector's value."
    (connector "The connector to set"))
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  @("Forgets this connector's value."
    (connector "The connector to forget"))
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  @("Connects a connector to a new constraint."
    (connector "The connector to connect")
    (new-constraint "The constraint to add"))
  ((connector 'connect) new-constraint))

(define (inform-about-value constraint)
  @("Informs the constraint about a new value"
    (constraint "The constraint to inform"))
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  @("Informs the constraint about forgetting."
    (constraint "The consraint to inform"))
  (constraint 'I-lost-my-value))

(define (adder a1 a2 sum)
  @("A constraint that adds two numbers"
    (a1 "Addend")
    (a2 "Augend")
    (sum "Sum")
    (@to "constraint"))
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  @("A constraint that multiplies two numbers"
    (a1 "Multiplier")
    (a2 "Multiplicand")
    (sum "Product")
    (@to "constraint"))  
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  @("A constant constraint"
    (value "The value to constantly be")
    (connector "The relevant connector")
    (@to "constraint"))
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  @("Probes a connector and informs upon value-change."
    (name "Name of the connector")
    (connector "The connector to probe")
    (@to "constraint"))
  (define (print-probe value)
    (format #t "Probe: ~a = ~a~%" name value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  @("Makes a connector."
    (@to "connector"))
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (case request
        ((has-value?) (and informant #t))
        ((value) value)
        ((set-value!) set-my-value)
        ((forget) forget-my-value)
        ((connect) connect)
        (else (error "Unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  @("Applies a procedure to every item in ''list'' except ones {{eq?}}
to ''exception''."
    (exception "An element not to apply ''procedure'' to")
    (procedure "The procedure to apply")
    (list "The list to iterate over"))  
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))
