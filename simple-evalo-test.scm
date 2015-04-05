(load "backjumping.scm")

(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define errorf
  (lambda (tag . args)
    (printf "Failed: ~s: ~%" tag)
    (apply printf args)
    (error 'WiljaCodeTester "That's all, folks!")))

(define-syntax test-check
  (syntax-rules ()
                ((_ title tested-expression expected-result)
                 (begin
                   (cout "Testing " title nl)
                   (let* ((expected expected-result)
                          (produced tested-expression))
                     (or (equal? expected produced)
                         (errorf 'test-check
                                 "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                                 'tested-expression expected produced)))))))


(define (evalo e v)
  (conde
    [(conde
       [(== #t e)]
       [(== #f e)])
     (== e v)]
    [(fresh (e1 e2 v1 v2)
       (== `(cons ,e1 ,e2) e)
       (== `(,v1 . ,v2) v)
       (evalo e1 v1)
       (evalo e2 v2))]
    [(fresh (inner inner-v d)
       (== `(car ,inner ,d) e)
       (evalo inner inner-v)
       (== `(,v . ,d) inner-v)
       )]))

(run 1000 (q) (evalo q #t))
