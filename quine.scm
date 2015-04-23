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

;;; Helpers

;; Disequality for Peano Numbers
#|
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
         (== n2 `(s ,n2-1)))))))
|#

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

;;; Normalization (of terms, so that they can be evaluated in Scheme)
;; Our language is a subset of Scheme, except that we need to turn
;; our peano-encoded variables into Scheme symbols.

;; Normalizes the list (s ...(s <x>)) to s...s<x>,
;; where s can be applied 0 or more times to the symbol <x>,
;; which will be either z or an unbound logic variable such as _.0.

(define (assert p)
  (if (not p)
    (error "error")
    (void)))

(define normalize-var-name
  (lambda (n)
    (if (and (list? n) (eq? 's (car n)) (null? (cddr n)))
      (string->symbol (string-append
                        (symbol->string (car n))
                        (symbol->string (normalize-var-name (cadr n)))))
      (begin
        (assert (symbol? n))
        n))))

;; Normalizes all occurrences of (vr <peano>) to a symbol.
(define normalize
  (lambda (t)
    (if (list? t)
      (if (and (eq? 'vr (car t)) (null? (cddr t)))
        (normalize-var-name (cadr t))
        (map normalize t))
      (begin
        (assert (not (eq? 'vr t)))
        t))))

;;;; Tests

(define ok
  (lambda (r)
    (assert (not (null? r)))
    r))

;; Quine verification.
(define quine
  '((lambda ((vr z)) (list (vr z) (list (quote quote) (vr z))))
    (quote (lambda ((vr z)) (list (vr z) (list (quote quote) (vr z)))))))

;(ok
  ;(normalize
    ;(run* (q)
      ;(ev '()
          ;quine
          ;`(code ,quine)))))

;;;; Quine generation.

(call/cc
  (lambda (k)
    (with-exception-handler
      (lambda (e) (k))
      (lambda ()
        (time (begin
                (display "\n")
                (display (length (normalize
                                   (run 13 (q)
                                        (ev '()
                                            q
                                            `(code ,q))))))
                (display "\n")
                (display count)
                (display "\n")))))))



;;; Twine generation.
;(ok
;(map
;(lambda (ab)
;(let ((a (car ab))
;(b (cadr ab)))
;(assert (equal? (eval a) b))
;(assert (equal? (eval b) a))
;(list a b)))
;(normalize
;(run 1 (q)
;(fresh (a b)
;(== q `(,a ,b))
;(ev '() a `(code ,b))
;(ev '() b `(code ,a)))))))



;;;; shadowing tests
;(ok
;(run 1 (q)
;(ev '()
;'(((lambda ((vr z)) (lambda ((vr z)) (vr z))) (quote 5)) (quote 6))
;q)))

;(ok
;(run 1 (q)
;(ev '()
;'(((lambda ((vr z)) (lambda ((vr z)) (vr z))) (quote 6)) (quote 5))
;q)))


;(ok
;(run 1 (q)
;(ev '()
;q
;'(code (I love you)))))
