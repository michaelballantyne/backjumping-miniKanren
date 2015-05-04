(load "test-check.scm")

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

(test-check "simple evalo"
  (time (length (run 1000 (q) (evalo q #t))))
  1000)
