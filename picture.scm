(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (add-vect v1 (make-vect (- (xcor-vect v2))
                          (- (ycor-vect v2)))))

(define (scale-vect s v1)
  (make-vect (* s (xcor-vect v1))
             (* s (ycor-vect v1))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1 0.5)
                     (make-vect 0.5 1)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0 0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
;; From 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

;; Modified this (see also
;; <http://eli.thegreenplace.net/2007/08/24/sicp-section-224/>)
;; because it appears to be rotating clockwise not anti-clockwise.
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define rotate180 (compose rotate90 rotate90))
(define rotate270 (compose rotate180 rotate90))

;; From 2.51
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0 0)
            split-point
            (make-vect 0 1)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1 0)
            (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; Call it svg-width or image-width?
(define image-width (make-parameter 256))

(define image-height (make-parameter 256))

(define image-frame
  (make-parameter (lambda () (make-frame (make-vect 0 0)
                                    (make-vect (image-width) 0)
                                    (make-vect 0 (image-height))))))

(define svg-document
  `(svg (,at (xmlns "http://www.w3.org/2000/svg")
             (xmlns:xlink "http://www.w3.org/1999/xlink")
             (height (%data height))
             (width (%data width)))
        (g (,at (stroke "black"))
           (%data body))))

(define display-features-as-svg
  (case-lambda ((features)
           (display-features-as-svg features (image-width) (image-height)))
          ((features width height)
           (write-shtml-as-html
            (substitute-tokens svg-document
                               `((height . ,(number->string height))
                                 (width . ,(number->string width))
                                 (body ,features)))))))

(define (make-accumulator)
  (let ((features '()))
    (case-lambda
     (() features)
     ((feature)
      (set! features (cons feature features))))))

(define accumulator (make-parameter (make-accumulator)))

(define (line->svg start end)
  `(line (,at (x1 ,(number->string (xcor-vect start)))
              (y1 ,(number->string (ycor-vect start)))
              (x2 ,(number->string (xcor-vect end)))
              (y2 ,(number->string (ycor-vect end))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
        (lambda (segment)
          ((accumulator)
           (line->svg
            ((frame-coord-map frame) (start-segment segment))
            ((frame-coord-map frame) (end-segment segment)))))
      segment-list)))

(define (image->painter image)
  (lambda (frame)
    (let ((origin (origin-frame frame))
          (edge1 (edge1-frame frame))
          (edge2 (edge2-frame frame)))
      (let ((a (car edge1))
            (b (cdr edge1))
            (c (car edge2))
            (d (cdr edge2))
            (e (car origin))
            (f (cdr origin)))
        ((accumulator)
         `(g (,at (transform
                   ,(format "matrix(~a, ~a, ~a, ~a, ~a, ~a)" a b c d e f)))
             (image (,at (xlink:href ,image)
                         (width "1")
                         (height "1")))))))))

(define (draw-painter-as-svg painter)
  (parameterize ((accumulator (make-accumulator)))
    (painter ((image-frame)))
    (display-features-as-svg ((accumulator)))))

(define (write-painter-to-svg painter svg)
  (with-output-to-file svg
    (lambda () (draw-painter-as-svg painter))))

(define (write-painter-to-png painter png)
  (let ((svg (create-temporary-file ".svg")))
    (with-output-to-file svg
      (lambda () (draw-painter-as-svg painter)))
    (run (inkscape -z -e ,png ,svg))))

;; Define a basic shape we can test against.
(define outline
  (list
   (make-segment (make-vect 0 0) (make-vect 0 1))
   (make-segment (make-vect 0 1) (make-vect 1 1))
   (make-segment (make-vect 1 1) (make-vect 1 0))
   (make-segment (make-vect 1 0) (make-vect 0 0))))
