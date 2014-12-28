(use srfi-18)

(define (thunk->thread thunk)
  @("Creates a thread from {{thunk}} and start the thread."
    (thunk "The thunk to threadify")
    (@to "thread"))
  (let ((thread (make-thread thunk)))
    (thread-start! thread)
    thread))

(define (parallel-execute . thunks)
  @("Executes thunks in parallel; returns a thunk which can be executed
to terminate the threads."
    (thunks "The thunks to execute in parallel")
    (@to "thunk"))
  (let ((threads (map thunk->thread thunks)))
    (lambda ()
      (for-each thread-terminate! threads))))

(define with-mutex-locked
  @("Evaluates the thunk having locked the mutex, unlocking it thereafter."
    (mutex "The mutex to lock and unlock")
    (thunk "The thunk to evaluate")
    (conditional-variable "An optional conditional-variable to block
on at unlock")
    (@to "object"))
  (case-lambda ((mutex thunk)
           (with-mutex-locked mutex thunk #f))
          ((mutex thunk conditional-variable)
           (dynamic-wind
               (lambda () (mutex-lock! mutex))
               thunk
               (lambda () (mutex-unlock! mutex conditional-variable))))))

(define (make-serializer)
  @("Creates a serializer which returns serialized procedures in a
common set; returns a procedure taking {{f}}, the procedure to
serialize."
    (@to "procedure")
    (@example "Create a serializer and run some thunks."
              (let ((s (make-serializer))
                    (x 10))
                (parallel-execute
                 (s (lambda () (set! x (* x x))))
                 (s (lambda () (set! x (+ x 1))))))))
  (let ((mutex (make-mutex)))
    (lambda (f)
      (lambda args
        (with-mutex-locked mutex (lambda () (apply f args)))))))
