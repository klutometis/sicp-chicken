(module sicp
  (=number?
   accumulate
   add-vect
   addend
   augend
   average
   below
   beside
   corner-split
   dec
   default-timeout
   deriv
   draw-painter-as-svg
   edge1-frame
   edge2-frame
   end-segment
   enumerate-interval
   epsilon
   fast-prime?
   flatmap
   flip-horiz
   flip-vert
   frame-coord-map
   image->painter
   image-frame
   image-height
   image-width
   inc
   good-enough?
   make-frame
   make-product
   make-sum
   make-segment
   make-vect
   multiplicand
   multiplier
   nil
   origin-frame
   outline
   prime?
   product?
   right-split
   rotate90
   rotate180
   rotate270
   same-variable?
   scale-vect
   segments->painter
   shrink-to-upper-right
   square
   square-limit
   start-segment
   sub-vect
   sum?
   terminates?
   timeout-value?
   transform-painter
   up-split
   variable?
   write-painter-to-svg
   xcor-vect
   xor
   ycor-vect)
  (import chicken data-structures extras scheme)
  (use (only htmlprag write-shtml-as-html)
       srfi-18
       token-substitution)

  (include "differentiation.scm")
  (include "picture.scm")

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
