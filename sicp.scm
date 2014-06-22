(module sicp
  (average
   dec
   epsilon
   inc
   good-enough?
   square
   xor)
  (import chicken scheme)

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
               (not (and ,x ,y))))))))
