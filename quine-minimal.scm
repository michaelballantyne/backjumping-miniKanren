;; WEB -- 20 Jan 02015
;; fixed neq definition
;; replaced sort with list-sort in mk.scm to make it Vicare-compatible
;;
;; This code may still not be correct, due to overlapping between the
;; 2-argument 'list' built-in and the application clause.  Also, I'm
;; not sure about the handling of quote, especially without the
;; presence of absento to prevent quoted closures.  The distinction
;; between 'code' and 'clos' for the output value might help, although
;; I'm not sure I understand this distinction.

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

;;; Syntax

;; Peano numbers
(define nat
  (lambda (o)
    (conde
      ((== o 'z))
      ((fresh (n)
         (== o `(s ,n))
         (nat n))))))

;; Terms
(define tm
  (lambda (o)
    (conde
      ((fresh (n)
         (== o `(vr ,n))
         (nat n)))
      ((== o 'quote))
      ((fresh (n t)
         (== o `(lambda ((vr ,n)) ,t))
         (nat n)
         (tm t)))
      ((fresh (t1 t2)
         (== o `(,t1 ,t2))
         (tm t1)
         (tm t2)))
      ((fresh (t1 t2)
         (== o `(list ,t1 ,t2))
         (tm t1)
         (tm t2))))))

;; Values
(define vl
  (lambda (o)
    (conde
      ((fresh (e n t)
         (== o `(clo ,e ,n ,t))
         (venv e)
         (nat n)
         (tm t)))
      ((fresh (t)
         (== o `(code ,t))
         (tm t))))))

;; Environment
(define venv
  (lambda (o)
    (conde
      ((== o '()))
      ((fresh (n v e)
         (== o (cons `(,n ,v) e))
         (nat n)
         (vl v)
         (venv e))))))

(define neq
  (lambda (n1 n2)
    (conde
      ((== n1 'z)
       (fresh (n2-1)
         (== n2 `(s ,n2-1))))
      ((== n2 'z)
       (fresh (n1-1)
         (== n1 `(s ,n1-1))))
      ((fresh (n1-1 n2-1)
         (== n1 `(s ,n1-1))
         (== n2 `(s ,n2-1))
         (neq n1-1 n2-1))))))

;; Environment Lookup (where keys are peano numbers)
(define vlookup
  (lambda (e x v)
    (conde
      ((fresh (er)
         (== e (cons `(,x ,v) er))))
      ((fresh (y vy er)
         (== e (cons `(,y ,vy) er))
         (neq x y)
         (vlookup er x v))))))

;;; Evaluation
(define ev
  (lambda (e t v)
    (conde
      ((fresh (x)
         (== t `(vr ,x))
         (vlookup e x v)))
      ((fresh (x t0)
         (== t `(lambda ((vr ,x)) ,t0))
         (== v `(clo ,e ,x ,t0))))
      ((fresh (t0)
         (== t `(quote ,t0))
         (== v `(code ,t0))))
      ((fresh (t1 t2 e0 x0 t0 v2)
         (== t `(,t1 ,t2))
         (ev e t1 `(clo ,e0 ,x0 ,t0))
         (ev e t2 v2)
         (ev (cons `(,x0 ,v2) e0) t0 v)))
      ((fresh (t1 t2 c1 c2)
         (== t `(list ,t1 ,t2))
         (ev e t1 `(code ,c1))
         (ev e t2 `(code ,c2))
         (== v `(code ,(list c1 c2))))))))


(test-check "to5"
  (length (run 100 (q)
       (ev '()
           `(list ,q '6)
           '(code (5 6)))))
  100)
