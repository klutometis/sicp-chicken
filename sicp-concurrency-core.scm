(use srfi-18)

(define (thunk->thread thunk)
  @("Create a thread from {{thunk}} and start the thread."
    (thunk "The thunk to threadify")
    (@to "thread"))
  (let ((thread (make-thread thunk)))
    (thread-start! thread)
    thread))

(define (parallel-execute . thunks)
  @("Execute thunks in parallel; returns a thunk which can be executed
to terminate the threads."
    (thunks "The thunks to execute in parallel")
    (@to "thunk"))
  (let ((threads (map thunk->thread thunks)))
    (lambda ()
      (for-each thread-terminate! threads))))

(define (with-mutex-locked mutex thunk)

  @("Evaluate the thunk having locked the mutex and unlocking it thereafter."
    (mutex "The mutex to lock and unlock")
    (thunk "The thunk to evaluate")
    )

  (dynamic-wind
      (lambda () (mutex-lock! mutex))
      thunk
      (lambda () (mutex-unlock! mutex))))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (f)
      (lambda args
        (with-mutex-locked mutex (lambda () (apply f args)))))))
