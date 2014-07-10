(use sicp test)

(test
 "draw-painter-as-svg"
 "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"256\" width=\"256\"><g stroke=\"black\"><line x1=\"0.0\" y1=\"256.0\" x2=\"256.0\" y2=\"0.0\"></line></g></svg>"
 (with-output-to-string
   (lambda ()
     (draw-painter-as-svg (segments->painter
                           (list (make-segment (make-vect 0.0 1.0)
                                               (make-vect 1.0 0.0))))))))
