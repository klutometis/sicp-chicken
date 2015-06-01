@(heading "SICP")

(module sicp
  @(text "SICP is a grab-bag of different procedures from sections 1
through 3.3.4, before we started modularizing them (starting from
3.3.5: Constraints).")
  (=number?
   accumulate
   add
   add-action!
   add-to-agenda!
   add-vect
   addend
   adjoin-set
   after-delay
   and-gate
   and-gate-delay
   angle
   apply-generic
   attach-tag
   augend
   average
   below
   beside
   call-each
   choose-branch
   contents
   corner-split
   dec
   decode
   default-timeout
   delete-queue!
   deriv
   dispatch-table
   div
   draw-painter-as-svg
   edge1-frame
   edge2-frame
   element-of-set?
   empty-queue?
   encode
   encode-symbol
   end-segment
   enumerate-interval
   epsilon
   fast-prime?
   first-agenda-item
   flatmap
   flip-horiz
   flip-vert
   frame-coord-map
   front-ptr
   front-queue
   full-adder
   get
   get-signal
   half-adder
   huffman-adjoin-set
   image->painter
   image-frame
   image-height
   image-width
   imag-part
   inc
   insert-queue!
   install-complex-package
   install-polar-package
   install-rational-package
   install-rectangular-package   
   install-scheme-number-package
   intersection-set
   inverter
   inverter-delay
   good-enough?
   leaf?
   left-branch
   logical-not
   magnitude
   make-agenda
   make-code-tree
   make-complex-from-mag-ang
   make-complex-from-real-imag
   make-from-mag-ang
   make-from-real-imag
   make-frame
   make-leaf
   make-leaf-set
   make-product
   make-queue
   make-rational
   make-scheme-number
   make-sum
   make-segment
   make-vect
   make-wire
   mul
   multiplicand
   multiplier
   nil
   or-gate
   or-gate-delay
   origin-frame
   outline
   prime?
   probe
   product?
   propagate
   put
   real-part
   rear-ptr
   remove-first-agenda-item!
   right-branch
   right-split
   rotate90
   rotate180
   rotate270
   same-variable?
   scale-vect
   segments->painter
   set-front-ptr!
   set-rear-ptr!
   set-signal!
   shrink-to-upper-right
   square
   square-limit
   start-segment
   sub
   sub-vect
   sum?
   symbol-leaf
   symbols
   terminates?
   the-agenda
   time+values
   timeout-value?
   transform-painter
   type-tag
   up-split
   variable?
   weight
   weight-leaf
   write-painter-to-svg
   write-painter-to-png
   xcor-vect
   xor
   ycor-vect)
  (import chicken data-structures extras scheme)
  (use files
       (only hahn at)
       (only htmlprag write-shtml-as-html)
       shell
       srfi-18
       srfi-69
       token-substitution)

  (include "sicp-core.scm")
  (include "abstract-data.scm")
  (include "arithmetic.scm")
  (include "differentiation.scm")
  (include "huffman.scm")
  (include "picture.scm")
  (include "sets.scm")
  (include "queue.scm")
  (include "circuits.scm")

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
