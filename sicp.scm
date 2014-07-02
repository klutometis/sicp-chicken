(module sicp
  (accumulate
   average
   dec
   default-timeout
   enumerate-interval
   epsilon
   fast-prime?
   flatmap
   inc
   good-enough?
   nil
   prime?
   square
   terminates?
   timeout-value?
   xor)
  (import chicken extras scheme)
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
    (accumulate append nil (map proc seq)))

  (define (smallest-divisor n) (find-divisor n 2))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b) (= (remainder b a) 0))

  (define (prime? n)
    (= n (smallest-divisor n)))

  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))

  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f))))
