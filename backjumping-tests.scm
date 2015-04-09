(load "backjumping2.scm")

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

(test-check "walk-f-1"
  (let-values (((v v-f) (let ([x (var 'x)]
                              [y (var 'y)])
                          (walk-f x `((,x a 0)) 1))))
              (list v v-f))
  (list 'a 0))


(let ([x (var 'x)]
      [y (var 'y)])
  (test-check "unify-1"
    (let-values (((s s-f) (unify x y '() 1 0)))
                (list s s-f))
    (list `((,x ,y 1)) #f)))


(let ([x (var 'x)]
      [y (var 'y)])
  (test-check "unify-2"
    (let-values (((s s-f) (unify `(,x . ,y) `(,y . b) `((,y a 1)) 2 1)))
                (list s s-f))
    (list #f 1)))


(let ([x (var 'x)]
      [y (var 'y)])
  (test-check "unify-3"
    (let-values (((s s-f) (unify `(,x ,y ,x) `(,y b a) '() 1 0)))
                (list s s-f))
    (list #f 0)))

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
       (evalo e2 v2))]))

(test-check "cons-1"
  (run* (q) (fresh (e v)
              (== q `(,e ,v))
              (evalo `(cons ,e #t) `(,v . #f))))
  '())

(test-check "cons-2"
  (run 2 (q) (fresh (e v)
              (== q `(,e ,v))
              (evalo `(cons ,e #t) `(,v . #t))))
  '((#t #t) (#f #f)))

(define (alwayso)
  (conde
    [(== #t #t)]
    [(alwayso)]))

(test-check "always-1"
  (run* (q)
    (alwayso)
    (== #t #f))
  '())

(test-check "always-2"
  (run 5 (q)
       (alwayso)
       (== q #t))
  '(#t #t #t #t #t))

(test-check "always-occurs-check"
  (run* (q)
    (alwayso)
    (== q (list q)))
  '())

(test-check "always-3"
  (run 50 (q)
       (conde
         [(alwayso)
          (== q #f)
          (== #t #f)]
         [(alwayso)
          (==  q #t)]))
  '(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
    #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
    #t #t #t #t #t #t #t #t #t #t #t))

(define (always-exto x)
  (conde
    [(== x #t)]
    [(always-exto x)]))

(test-check "always-ext-1"
  (run* (q)
    (always-exto q)
    (== #t #f))
  '())

(test-check "always-ext-2"
  (run 5 (q)
    (fresh (x)
      (always-exto x)
      (== q #t)))
  '(#t #t #t #t #t))

(test-check "always-ext-occurs-check"
  (run* (q)
    (fresh (x)
      (always-exto x)
      (== q (list q))))
  '())

(test-check "always-ext-3"
  (run 50 (q)
    (fresh (x)
      (conde
        [(always-exto x)
         (== q #f)
         (== #t #f)]
        [(always-exto x)
         (==  q #t)])))
  '(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
    #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t
    #t #t #t #t #t #t #t #t #t #t #t))


(run* (x)
  (fresh (y)
    (fresh ()
      (conde
        [(== x 1)]
        [(== x 1)])
      (== x y))
    (== y 2)))
