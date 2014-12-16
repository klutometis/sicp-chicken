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

(define (stream->list stream)
  @("Converts a stream to a list, consuming it."
    (stream "The stream to convert to a list")
    (@to "stream"))
  (if (stream-null? stream)
      '()
      (cons (stream-car stream) (stream->list (stream-cdr stream)))))
