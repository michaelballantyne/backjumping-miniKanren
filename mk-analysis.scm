;;; This file was generated by writeminikanren.pl
;;; Generated at 2007-10-25 15:24:42

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (p) e) (lambda (p) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x0 x ...) g ...) (run #f (x0 x ...) g ...))))

(define-syntax rhs
  (syntax-rules ()
    ((_ x) (car (cdr x)))))

(define-syntax lhs
  (syntax-rules ()
    ((_ x) (car x))))

(define-syntax size-s
  (syntax-rules ()
    ((_ x) (length x))))

(define-syntax var
  (syntax-rules ()
    ((_ x) (vector x))))

(define-syntax var?
  (syntax-rules ()
    ((_ x) (vector? x))))

(define empty-s '())

(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (pr) (walk (rhs pr) S)))
      (else u))))

(define walk-f
  (lambda (u S min-jump)
    (cond
      ((and (var? u) (assq u S)) =>
       (lambda (a)
         (let-values ([(r r-f) (walk-f (rhs a) S min-jump)])
           (let ([assoc-f (car (cdr (cdr a)))]
                 [other-f (car (cdr (cdr (cdr a))))])
             (values r (max (if (> other-f min-jump) 0 other-f)
                         (max r-f (if (> assoc-f min-jump) 0 assoc-f))))))))
      (else (values u 0)))))

(define ext-s
  (lambda (x v version s jump)
    (cons `(,x ,v ,version ,jump) s)))

(define unify
  (lambda (u^ v^ s version min-jump)
    (let-values ([(u u-f) (walk-f u^ s min-jump)]
                 [(v v-f) (walk-f v^ s min-jump)])
      (cond
        ((eq? u v) (values s #f))
        ((var? u) (ext-s-check u v version min-jump s (max u-f v-f)))
        ((var? v) (ext-s-check v u version min-jump s (max u-f v-f)))
        ((and (pair? u) (pair? v))
         (let-values ([(car-s car-f)
                       (unify (car u) (car v) s version min-jump)])
           (if (not car-s)
             (values #f (max car-f (max u-f v-f)))
             (let-values ([(cdr-s cdr-f)
                           (unify (cdr u) (cdr v) car-s version min-jump)])
               (values cdr-s
                       (if cdr-s
                         #f
                         (max cdr-f (max u-f v-f))))))))
        ((equal? u v) (values s #f))
        (else
          (values #f (max u-f v-f)))))))

(define ext-s-check
  (lambda (x v version min-jump s jump)
    (let ([check-reason (occurs-check x v s min-jump)])
      (cond
        (check-reason (values #f (max check-reason jump)))
        (else (values (ext-s x v version s jump) #f))))))

(define occurs-check
  (lambda (x v s min-jump)
    (let-values ([(v v-f) (walk-f v s min-jump)])
      (cond
        [(and (var? v) (eq? v x)) v-f]
        [(pair? v)
         (or
           (occurs-check x (car v) s min-jump)
           (occurs-check x (cdr v) s min-jump))]
        [else #f]))))


(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
           (walk* (car v) s)
           (walk* (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v)
         (ext-s v (reify-name (size-s s)) 0 s 0))
        ((pair? v) (reify-s (cdr v)
                     (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
      (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v s)
    (let ((v (walk* v s)))
      (walk* v (reify-s v empty-s)))))

(define-syntax mzero 
  (syntax-rules () ((_) #f)))

(define-syntax inc 
  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define-syntax unit 
  (syntax-rules () ((_ a) a)))

(define-syntax choice 
  (syntax-rules () ((_ a f) (cons a f))))
 
(define-syntax case-inf
  (syntax-rules ()
    ((_ e (() e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((not a-inf) e0)
         ((procedure? a-inf)  (let ((f^ a-inf)) e1))
         ((not (and (pair? a-inf)
                    (procedure? (cdr a-inf))))
          (let ((a^ a-inf)) e2))
         (else (let ((a (car a-inf)) (f (cdr a-inf))) 
                 e3)))))))

(define count 0)
(define failed 0)

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (begin
       (set! count 0)
       (set! failed 0)
       (let ([x (var 'x)])
         (map
           (lambda (s)
             (reify x s))
           (take n
                 (lambdaf@ ()
                           ((fresh () g0 g ...)
                            empty-s)))))))
    ((_ n (x0 x ...) g0 g ...)
     (run n (q)
          (fresh (x0 x ...)
            (== q (list x0 x ...))
            g0 g ...)))))

(define take
  (lambda (n f)
    (if (and n (zero? n))
      '()
      (case-inf (f)
        (() '())
        ((f) (take n f))
        ((a) (list a))
        ((a f)
         (cons a
           (take (and n (- n 1)) f)))))))


(define ==
  (lambda (u v)
    (lambdag@ (s)
      (begin
        (set! count (+ 1 count))
        (let-values ([(r r-f) (unify u v s 1 0)])
          (if r
            r
            (begin
              (set! failed (+ 1 failed))
              #f)))))))
 
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s)
       (inc
         (let ((x (var 'x)) ...)
           (bind* (g0 s) g ...)))))))
 
(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...) (bind* (bind e g0) g ...))))
 
(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (() (mzero))
      ((f) (inc (bind (f) g)))
      ((a) (g a))
      ((a f) (mplus (g a) (lambdaf@ () (bind (f) g)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s) 
       (inc 
         (mplus* 
           (bind* (g0 s) g ...)
           (bind* (g1 s) g^ ...) ...))))))
 
(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0 
                    (lambdaf@ () (mplus* e ...))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (() (f))
      ((f^) (inc (mplus (f) f^)))
      ((a) (choice a f))
      ((a f^) (choice a (lambdaf@ () (mplus (f) f^)))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
       (inc
         (ifa ((g0 s) g ...)
              ((g1 s) g^ ...) ...))))))

(define-syntax ifa
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifa b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* a-inf g ...)))))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
       (inc
         (ifu ((g0 s) g ...)
              ((g1 s) g^ ...) ...))))))

(define-syntax ifu
  (syntax-rules ()
    ((_) (mzero))
    ((_ (e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
         (() (ifu b ...))
         ((f) (inc (loop (f))))
         ((a) (bind* a-inf g ...))
         ((a f) (bind* (unit a) g ...)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g g* ...)
     (lambdag@ (s)
       (let ((x (walk* x s)) ...)
         ((fresh () g g* ...) s))))))

(define succeed (== #f #f))

(define fail (== #f #t))

(define onceo
  (lambda (g)
    (condu
      (g succeed)
      ((== #f #f) fail))))


; Run interface for timing tests on large queries

(define (time-millis t)
  (+ (* (time-second t) 1000)
     (/ (time-nanosecond t) 1000000)))

(define (elapsed t1 t2)
  (exact->inexact (-
                    (time-millis t2)
                    (time-millis t1))))

(define take-inc
  (lambda (n orig-t last-t i f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() (begin
                          (printf "Failed after ~a second(s)\n" (elapsed orig-t))
                          '()))
         ((f) (take-inc n orig-t last-t i f))
         ((c) (let ([this-t (current-time)])
                (begin
                  (printf "~a\t~a\t~a\n" i (elapsed orig-t this-t) (elapsed last-t this-t))
                  ; (pretty-print c)
                  ; (printf "\n")
                  (cons c '()))))
         ((c f) (let ([this-t (current-time)])
                  (begin
                    (printf "~a\t~a\t~a\n" i (elapsed orig-t this-t) (elapsed last-t this-t))
                    ; (pretty-print c)
                    ; (printf "\n")
                    (cons c
                          (take-inc (and n (- n 1)) orig-t this-t (+ i 1) f))))))))))

(define-syntax run-inc
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (let ([t (current-time)])
       (take-inc n t t 1
                 (lambdaf@ ()
                           ((fresh (x) g0 g ...
                              (lambdag@ (s)
                                        (cons (reify x s) '())))
                            empty-s)))))))

(define-syntax run-inc*
  (syntax-rules ()
    ((_ (x) g ...) (run-inc #f (x) g ...))))
