(module sicp
  (accumulate
   average
   dec
   default-timeout
   enumerate-interval
   epsilon
   flatmap
   inc
   good-enough?
   nil
   square
   terminates?
   timeout-value?
   xor)
  (import chicken scheme)
  (use srfi-18)

  (define inc add1)

  (define dec sub1)

  (define epsilon (make-parameter 0.00001))
  
  (define (square x) (* x x))
  
  (define (good-enough? old-guess guess)
    (< (abs (- old-guess guess)) (epsilon)))
  
  (define (average x y) (/ (+ x y) 2))

  (define-syntax xor
    (ir-macro-transformer
     (lambda (expression inject compare)
       (let ((x (cadr expression))
             (y (caddr expression)))
         `(and (or ,x ,y)
               (not (and ,x ,y)))))))

  (define default-timeout (make-parameter 1))
  (define-record timeout-value)
  (define timeout-value (make-timeout-value))
  
  (define terminates?
    (case-lambda
     ((thunk) (terminates? thunk (default-timeout)))
     ((thunk timeout)
      (let ((thread (make-thread thunk)))
        (thread-start! thread)
        (not
         (timeout-value?
          (thread-join! thread timeout timeout-value)))))))

  (define nil '())

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

  (define (flatmap proc seq)
    (accumulate append nil (map proc seq))))
