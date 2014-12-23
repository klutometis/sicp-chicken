(use sicp sicp-streams test)

(test
 "draw-painter-as-svg"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"256\" width=\"256\"><g stroke=\"black\"><line x1=\"0.0\" y1=\"256.0\" x2=\"256.0\" y2=\"0.0\"></line></g></svg>"
 (with-output-to-string
   (lambda ()
     (draw-painter-as-svg (segments->painter
                           (list (make-segment (make-vect 0.0 1.0)
                                               (make-vect 1.0 0.0))))))))

(test
 "draw-painter-as-svg: beside"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"256\" width=\"256\"><g stroke=\"black\"><line x1=\"128.0\" y1=\"256.0\" x2=\"256.0\" y2=\"0.0\"></line><line x1=\"0.0\" y1=\"256.0\" x2=\"128.0\" y2=\"0.0\"></line></g></svg>"
 (with-output-to-string
   (lambda ()
     (let ((painter (segments->painter
                     (list (make-segment (make-vect 0.0 1.0)
                                         (make-vect 1.0 0.0))))))
       (draw-painter-as-svg (beside painter painter))))))

(test
 "draw-painter-as-svg: image"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" height=\"256\" width=\"256\"><g stroke=\"black\"><g transform=\"matrix(128.0, 0.0, 0.0, 256.0, 128.0, 0.0)\"><image xlink:href=\"lena.png\" width=\"1\" height=\"1\"></image></g><g transform=\"matrix(128.0, 0.0, 0, 256, 0, 0)\"><image xlink:href=\"lena.png\" width=\"1\" height=\"1\"></image></g></g></svg>"
 (with-output-to-string
   (lambda ()
     (let ((painter (image->painter "lena.png")))
       (draw-painter-as-svg (beside painter painter))))))

(test 1 (deriv '(+ x 3) 'x))
(test 'y (deriv '(* x y) 'x))

(test '(1 2 3)
      (stream->list
       (cons-stream 1 (cons-stream 2 (cons-stream 3 stream-null)))))

(test '(1 2)
      (stream->list
       (cons-stream 1 (cons-stream 2 (cons-stream 3 stream-null)))
       2))
