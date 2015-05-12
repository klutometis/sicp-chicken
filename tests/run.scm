(use sicp sicp-eval sicp-eval-anal sicp-streams test)

(test
 "Draw-painter-as-svg"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"256\" width=\"256\"><g stroke=\"black\"><line x1=\"0.0\" y1=\"256.0\" x2=\"256.0\" y2=\"0.0\"></line></g></svg>"
 (with-output-to-string
   (lambda ()
     (draw-painter-as-svg (segments->painter
                           (list (make-segment (make-vect 0.0 1.0)
                                               (make-vect 1.0 0.0))))))))

(test
 "Draw-painter-as-svg: beside"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"256\" width=\"256\"><g stroke=\"black\"><line x1=\"128.0\" y1=\"256.0\" x2=\"256.0\" y2=\"0.0\"></line><line x1=\"0.0\" y1=\"256.0\" x2=\"128.0\" y2=\"0.0\"></line></g></svg>"
 (with-output-to-string
   (lambda ()
     (let ((painter (segments->painter
                     (list (make-segment (make-vect 0.0 1.0)
                                         (make-vect 1.0 0.0))))))
       (draw-painter-as-svg (beside painter painter))))))

(test
 "Draw-painter-as-svg: image"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"256\" width=\"256\"><g stroke=\"black\"><g transform=\"matrix(128.0, 0.0, 0.0, 256.0, 128.0, 0.0)\"><image xlink:href=\"lena.png\" width=\"1\" height=\"1\"></image></g><g transform=\"matrix(128.0, 0.0, 0, 256, 0, 0)\"><image xlink:href=\"lena.png\" width=\"1\" height=\"1\"></image></g></g></svg>"
 (with-output-to-string
   (lambda ()
     (let ((painter (image->painter "lena.png")))
       (draw-painter-as-svg (beside painter painter))))))

(test "Numerical derivation" 1 (deriv '(+ x 3) 'x))
(test "Symbolic derivation" 'y (deriv '(* x y) 'x))

(test "Stream->list with all"
      '(1 2 3)
      (stream->list
       (cons-stream 1 (cons-stream 2 (cons-stream 3 stream-null)))))

(test "Stream->list with 2"
      '(1 2)
      (stream->list
       (cons-stream 1 (cons-stream 2 (cons-stream 3 stream-null)))
       2))

(test "List->string"
      '(1 2 3)
      (stream->list (list->stream '(1 2 3))))

(test "The evaluator evaluates"
      5
      (parameterize ((primitive-procedures
                      (cons (list '+ +) (primitive-procedures))))
        (eval* '(+ 2 3) (setup-environment))))

(call-with-values (lambda () (time+values (+ 2 2)))
  (lambda (time value)
    (test-assert "Time returns a number" (number? time))
    (test "Values returns the value" 4 value)))

(test "The analyzing evaluator analyzes and evaluates"
      5
      (parameterize ((primitive-procedures
                      (cons (list '+ +) (primitive-procedures))))
        (anal-eval* '(+ 2 3) (setup-environment))))

(use sicp-eval-lazy)

(with-primitive-procedures `((+ ,+)
                             (values ,values))
  (lambda (env)
    (eval* '(define count 0) env)
    (eval* '(define (id x) (set! count (+ count 1)) x) env)
    (eval* '(define w (id (id 10))) env)
    (test "Id has been called once." 1 (eval* 'count env))
    (test "W is 10." 10 (eval* '(values w) env))
    (test "Id has been called twice." 2 (eval* 'count env))))

(test-exit)
