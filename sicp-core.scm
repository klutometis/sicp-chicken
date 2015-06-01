(define-syntax time+values
  @("Evaluates {{expressions}} and returns the time required for
evaluation plus the return values of evaluating {{expressions}}."
    (expressions "The expressions to evaluate")
    (@to "{number, [object, [object, ...]]}"))
  (ir-macro-transformer
   (lambda (expression rename inject)
     `(begin
        (##sys#start-timer)
        (call-with-values
            (lambda () ,@(cdr expression))
          (lambda return-values
            (let ((time (vector-ref (##sys#stop-timer) 0)))
              (apply values (cons time return-values)))))))))
