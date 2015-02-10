(define dispatch-table (make-parameter (make-hash-table)))

(define (put op type proc)
  (hash-table-set! (dispatch-table) (cons op type) proc))

;;; TODO: Get should take an optional default.
(define (get op type)
  (hash-table-ref/default (dispatch-table) (cons op type) #f))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))
