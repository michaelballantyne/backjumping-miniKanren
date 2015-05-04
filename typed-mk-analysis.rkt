#lang typed/racket/no-check

(provide (all-defined-out))

(struct var ([sym : Symbol]) #:transparent)


(define-type reason Nonnegative-Fixnum)

(struct
  association
  ([var : var]
   [term : Any]
   [added : reason]
   [because : reason])
  #:transparent)

(struct substitution ([l : (Listof association)]))

(: lookup (-> var (Listof association) (U association #f)))
(define (lookup v l)
  (if (pair? l)
    (if (eq? v (association-var (car l)))
      (car l)
      (lookup v (cdr l)))
    #f))

(: walk-f (-> Any substitution reason (values Any reason)))
(define (walk-f u s min-jump)
  (if (var? u)
    (let ([a (lookup u (substitution-l s))])
      (if a
        (let-values ([(r r-f) (walk-f (association-term a) s min-jump)])
          (values r (max (if (> (association-added a) min-jump)
                           0
                           (association-added a))
                         (max r-f
                              (if (> (association-because a) min-jump)
                                0
                                (association-because a))))))
          (values u 0)))
      (values u 0)))

(define-type unification-result (U substitution reason))

(: unify (-> Any Any substitution reason reason unification-result))
(define (unify u v s version min-jump)
  (let-values ([(u u-f) (walk-f u s min-jump)]
               [(v v-f) (walk-f v s min-jump)])
    (cond
      ((eq? u v) s)
      ((var? u) (ext-s-check u v version min-jump s (max u-f v-f)))
      ((var? v) (ext-s-check v u version min-jump s (max u-f v-f)))
      ((and (pair? u) (pair? v))
       (let ([car-s (unify (car u) (car v) s version min-jump)])
         (if (substitution? car-s)
           (let ([cdr-s (unify (cdr u) (cdr v) car-s version min-jump)])
             (if (substitution? cdr-s)
               cdr-s
               (max cdr-s (max u-f v-f))))
           (max car-s (max u-f v-f)))))
      ((equal? u v) s)
      (else (max u-f v-f)))))

(: ext-s-check (-> var Any reason reason substitution reason unification-result))
(define (ext-s-check x v version min-jump s jump)
  (let ([check-reason (occurs-check x v s min-jump)])
    (if check-reason
      (max check-reason jump)
      (ext-s x v version s jump))))

(: ext-s (-> var Any reason substitution reason substitution))
(define (ext-s x v version s jump)
  (substitution (cons (association x v version jump) (substitution-l s))))

(: occurs-check (-> var Any substitution reason (U reason #f)))
(define (occurs-check x v s min-jump)
  (let-values ([(v v-f) (walk-f v s min-jump)])
    (cond
      [(and (var? v) (eq? v x)) v-f]
      [(pair? v)
       (or
         (occurs-check x (car v) s min-jump)
         (occurs-check x (cdr v) s min-jump))]
      [else #f])))

(define v (var 'v))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(define s
  (substitution (list
                  (association (var 'a) 1 0 0)
                  (association (var 'a) 2 0 0)
                  (association (var 'a) 3 0 0)
                  (association (var 'a) 4 0 0)
                  (association (var 'a) 5 0 0)
                  (association (var 'a) 6 0 0)
                  (association x y 0 0)
                  (association (var 'a) 8 0 0)
                  (association (var 'a) 9 0 0)
                  (association (var 'a) 10 0 0)
                  (association (var 'a) 11 0 0)
                  (association (var 'a) 12 0 0)
                  (association (var 'a) 13 0 0)
                  (association v x 0 0)
                  (association (var 'a) 15 0 0)
                  (association (var 'a) 16 0 0)
                  (association (var 'a) 17 0 0)
                  (association y 18 0 0)
                  (association (var 'a) 19 0 0))))



(time (for ([i (in-range 10000000)])
        (unify z v s 1 0)))


