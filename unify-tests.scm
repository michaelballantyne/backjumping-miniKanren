(load "cps-noflatten-analysis2.scm")

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
