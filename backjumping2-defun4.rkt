#lang racket

(provide (all-defined-out))


(define-syntax run*
  (syntax-rules ()
    ((_ (x0 x ...) g ...) (run #f (x0 x ...) g ...))))

(define size-s hash-count)
(define var vector)
(define var? vector?)

(define empty-s (hasheq))


(define walk
  (lambda (u S)
    (cond
      ((and (var? u) (hash-has-key? S u))
       (walk (car (hash-ref S u)) S))
      (else u))))

(define walk-f
  (lambda (u S min-jump)
    (cond
      ((and (var? u) (hash-has-key? S u))
       (let ([a (hash-ref S u)])
         (let-values ([(r r-f) (walk-f (car a) S min-jump)])
           (let ([assoc-f (car (cdr a))]
                 [other-f (car (cdr (cdr a)))])
             (values r (max (if (> other-f min-jump) 0 other-f)
                         (max r-f (if (> assoc-f min-jump) 0 assoc-f))))))))
      (else (values u 0)))))

(define ext-s
  (lambda (x v version s jump)
    (hash-set s x (list v version jump))))

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


(struct failure (reason mode))
(struct result (value))
(struct choice (success tree))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e ((target mode) e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (match e
       [(failure target mode) e0]
       [(and a^ (result _)) e2]
       [(choice a f) e3]
       [f^ e1]))))

(struct conj (g1 g2))
(struct disj (g1 g2))
(struct == (u v))

(struct mplus (a-inf f version destructive-top))
(struct mplus-single (a-inf destructive-top other-target))

(struct construct-delayed (tree s k version min-jump destructive-top))

(define unifications 0)

(define (construct-stream tree s k version min-jump destructive-top depth)
  (if (> depth 0)
    (match tree
      [(== u v)
       (let-values ([(s s-f) (unify u v s version min-jump)])
         (set! unifications (+ unifications 1))
         (if s
           (k s version min-jump destructive-top)
           (failure s-f destructive-top)))]
      [(conj g1 g2)
       (construct-stream
         g1
         s
         (lambda (s bottom-version bottom-min-jump bottom-destructive-top)
           (let-values
             ([(version min-jump)
               ; check if the substitution has been extended in this
               ; subtree.
               (if (> bottom-destructive-top version)
                 ; if so, go to new version on exit and set
                 ; minimum jump to version established in this subtree.
                 (values (+ 1 bottom-version)
                         bottom-version)
                 ; if we didn't extend the substitution, leave the
                 ; version and min-jump alone to allow jumping past the subtree
                 (values bottom-version bottom-min-jump))])
             (construct-stream g2 s k version min-jump destructive-top depth)))
         version
         min-jump
         destructive-top
         depth)]
      [(disj g1 g2)
       (mplus
         (construct-stream g1 s k (+ 1 version) min-jump (+ 1 version) (- depth 1))
         (construct-delayed g2 s k (+ 1 version) min-jump (+ 1 version))
         version
         destructive-top)]
      [(? procedure? p)
       (construct-stream (p) s k version min-jump destructive-top depth)])
    (construct-delayed tree s k version min-jump destructive-top)))

(define num 0)

(define (process-stream s)
  (match s
    [(mplus a-inf f version destructive-top)
     (case-inf (process-stream a-inf)
      ((target mode)
       (if (and (<= target version) (<= mode version))
         ; Backjumping beyond here and have the mode to do it destructively.
         ; Propagate failure and if we have a more powerful destructive-top
         ; use that as new mode.
         (failure target (min mode destructive-top))

         ; Otherwise backjumping stalls here. Resume when we know what the
         ; other branch does. If the stalled jump wouldn't reach past here
         ; we'll resume the jump with this node's version.
         (mplus-single f destructive-top (min target version))))
      ((f^) (mplus f f^ version destructive-top))
      ((a) (choice a (mplus-single f destructive-top version)))
      ((a f^) (choice a (mplus f f^ version destructive-top))))]
    [(mplus-single a-inf destructive-top other-target)
     (case-inf (process-stream a-inf)
      ((target mode)
       (failure (max target other-target) (min mode destructive-top)))
      ((f^) (if (mplus-single? f^)
              (match-let ([(mplus-single a-inf^ destructive-top^ other-target^) f^])
                (mplus-single a-inf^ (min destructive-top destructive-top^) (max other-target other-target^)))
              (mplus-single f^ destructive-top other-target)))
      ((a) a)
      ((a f^) (choice a (mplus-single f^ destructive-top other-target))))]
    [(construct-delayed tree s k top-version top-min-jump top-destructive-top)
     (construct-stream tree s k top-version top-min-jump top-destructive-top 5)]
    [_ s]))


(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (let ((x (var 'x)) ...)
       (bind* g0 g ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (mplus*
       (bind* g0 g ...)
       (bind* g1 g^ ...) ...))))

(define-syntax bind*
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (bind* g ...)))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...)
     (disj
       (lambda () e0)
       (lambda () (mplus* e ...))))))


(define takemk
  (lambda (n f)
    (if (and n (zero? n))
      (begin (displayln unifications) '())
      (case-inf (process-stream f)
        ((target mode) '())
        ((f) (takemk n f))
        ((a) (list a))
        ((a f)
         (cons a
           (takemk (and n (- n 1)) f)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (let ([x (var 'x)])
       (map
         (lambda (s)
           (reify x (result-value s)))
         (takemk n
                 (construct-delayed
                   (bind* g0 g ...)
                   empty-s ; s
                   (lambda (s version min-jump destructive-top) (result s)) ; k
                   0 ; version
                   0 ; min-jump
                   0)))))
    ((_ n (x0 x ...) g0 g ...)
     (run n (q)
          (fresh (x0 x ...)
            (== q (list x0 x ...))
            g0 g ...)))))


(define succeed (== #f #f))
(define fail (== #f #t))


(define take-inc
  (lambda (n orig-t last-t i f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (process-stream f)
         ((target mode) (begin
                          (printf "Failed after ~a second(s)\n" (- orig-t (current-inexact-milliseconds)))
                          '()))
         ((f) (take-inc n orig-t last-t i f))
         ((c) (let ([this-t (current-inexact-milliseconds)])
                (begin
                  (printf "~a\t~a\t~a\n" i (- this-t orig-t) (- this-t last-t))
                  (flush-output (current-output-port))
                  ; (pretty-print c)
                  ; (printf "\n")
                  (cons c '()))))
         ((c f) (let ([this-t (current-inexact-milliseconds)])
                  (begin
                    (printf "~a\t~a\t~a\n" i (- this-t orig-t) (- this-t last-t))
                    (flush-output (current-output-port))
                    ; (pretty-print c)
                    ; (printf "\n")
                    (cons c
                          (take-inc (and n (- n 1)) orig-t this-t (+ i 1) f))))))))))

(define-syntax run-inc
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (let ([x (var 'x)])
       (let ([t (current-inexact-milliseconds)])
         (take-inc n t t 1
                   (construct-delayed
                     (bind* g0 g ...)
                     empty-s ; s
                     (lambda (s version min-jump destructive-top) (result s)) ; k
                     0 ; version
                     0 ; min-jump
                     0)))))))

(define-syntax run-inc*
  (syntax-rules ()
    ((_ (x) g ...) (run-inc #f (x) g ...))))
