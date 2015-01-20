(define stream-null
  @("The empty stream")
  '())

(define the-empty-stream
  @("A synonym for {{stream-null}}")
  stream-null)

(define stream-null?
  @("Is this stream null?"
    (stream "The stream to test")
    (@to "boolean"))
  null?)

(define-syntax cons-stream
  @("Constructs a stream; returns a stream whose {{stream-car}} is {{a}}
and whose {{stream-cdr}} is a delayed {{d}}."
    (a "The address part")
    (d "The decrement part")
    (@to "stream"))  
  (ir-macro-transformer
   (lambda (expression rename inject)
     (match expression
       ((_ a b) `(cons ,a (delay ,b)))))))

(define (stream-ref s n)
  @("Returns the nth element of the stream, consuming any non-memoized
elements."
    (s "The stream to consume")
    (n "The nth element")
    (@to "object"))
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  @("Constructs a stream which is a {{proc}}-mapped {{s}}."
    (proc "The procedure to apply")
    (s "The stream to apply to")
    (@to "stream"))
  (if (stream-null? s)
      stream-null
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  @("Applies {{proc}} to every element of {{s}}, consuming it."
    (proc "The procedure to apply")
    (s "The stream to apply to"))
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  @("Displays every element of the stream."
    (s "The stream to display"))
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-car stream)
  @("Takes the first element of the stream."
    (stream "The stream to take")
    (@to "object"))
  (car stream))

(define (stream-cdr stream)
  @("Forces and returns the cdr of the stream."
    (stream "The stream whose cdr to force")
    (@to "stream"))
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
  @("Enumerates the interval between {{low}} and {{high}} streamingly."
    (low "The lower bound")
    (high "The upper bound")
    (@to "stream"))
  (if (> low high)
      stream-null
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  @("Filters a stream, applying {{pred}}."
    (pred "The predicate upon which to filter.")
    (stream "The stream to filter")
    (@to "stream"))
  (cond ((stream-null? stream) stream-null)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define stream->list
  @("Converts a stream to a list, consuming it (or up to n elements)."
    (stream "The stream to convert to a list")
    (n "Optionally, the maximum number of elements to consume; otherwise: all elements")
    (@to "stream"))
  (case-lambda
   ((stream) (stream->list stream +inf.0))
   ((stream n)
    (if (or (stream-null? stream) (zero? n))
        '()
        (cons (stream-car stream) (stream->list (stream-cdr stream) (- n 1)))))))

(define (scale-stream stream factor)
  @("Scales the stream by a constant factor."
    (stream "The stream to scale")
    (factor "The factor by which to scale it")
    (@to "stream"))
  (stream-map (lambda (x) (* x factor)) stream))

(define (euler-transform s)
  @("Applies [[http://en.wikipedia.org/wiki/Series_acceleration#Euler.27s_transform|Euler's
transform]], i.e. a linear sequence transformation for improved
convergence, to a stream."
    (s "The stream to which to apply Euler's transform")
    (@to "stream"))
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  @("Makes a tableau (i.e., a stream of streams) compounded from some
transformation."
    (transform "The compounding transformation")
    (s "The stream to transformatively compound")
    (@to "stream"))
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  @("Accelerates some converging sequence."
    (transform "The transformation to apply")
    (s "The sequence to accelerate, e.g. [[euler-transform]]")
    (@to "stream"))
  (stream-map stream-car (make-tableau transform s)))

(define (integers-starting-from n)
  @("Enumerates the integers starting from n streamingly."
    (n "The number to start from")
    (@to "stream"))
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers
  @("Enumerates the positive integers streamingly.")
  (integers-starting-from 1))

(define (interleave s1 s2)
  @("Interleaves two streams."
    (s1 "The interleavened stream")
    (s1 "The interleaving stream")
    (@to "stream"))
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  @("Generates the stream of pairs (S_i, T_j), where i <= j."
    (s "The first stream to pair")
    (t "The second stream to pair")
    (@to "stream"))  
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (merge s1 s2)
  @("Merges two ordered streams into one ordered result stream,
eliminating repetitions."
    (s1 "Mergend")
    (s2 "Merger")
    (@to "stream"))
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define (list->stream list)
  @("Takes a list and streamifies it."
    (list "The list to streamify")
    (@to "stream"))
  (if (null? list) stream-null
      (cons-stream (car list) (list->stream (cdr list)))))

