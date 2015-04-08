(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s k version min-jump) e) (lambda (s k version min-jump) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

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
           (let ([assoc-f (car (cdr (cdr a)))])
             (values r (max r-f (if (> assoc-f min-jump) 0 assoc-f)))))))
      (else (values u 0)))))

(define ext-s
  (lambda (x v version s)
    (cons `(,x ,v ,version) s)))

(define unify
  (lambda (u^ v^ s version min-jump)
    (let-values ([(u u-f) (walk-f u^ s min-jump)]
                 [(v v-f) (walk-f v^ s min-jump)])
      (cond
        ((eq? u v) (values s #f))
        ((var? u) (ext-s-check u v^ version s (max u-f v-f)))
        ((var? v) (ext-s-check v u^ version s (max u-f v-f)))
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
  (lambda (x v version s jump)
    (cond
      ((occurs-check x v s) (values #f jump))
      (else (values (ext-s x v version s) #f)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v)
         (or
           (occurs-check x (car v) s)
           (occurs-check x (cdr v) s)))
        (else #f)))))

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
         (ext-s v (reify-name (size-s s)) 0 s))
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

(define-syntax inc
  (syntax-rules () ((_ e) (lambdaf@ () e))))

(define-syntax choice
  (syntax-rules () ((_ a f) (cons a f))))

(define-syntax failure
  (syntax-rules () ((_ target mode) (vector target mode))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e ((target mode) e0) ((f^) e1) ((a^) e2) ((a f) e3))
     (let ((a-inf e))
       (cond
         ((vector? a-inf)
          (let ((target (vector-ref a-inf 0))
                (mode (vector-ref a-inf 1)))
            e0))
         ((procedure? a-inf)  (let ((f^ a-inf)) e1))
         ((not (and (pair? a-inf)
                    (procedure? (cdr a-inf))))
          (let ((a^ a-inf)) e2))
         (else (let ((a (car a-inf)) (f (cdr a-inf)))
                 e3)))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ...
            (lambdag@ (s k version min-jump)
                      (cons (reify x s) '())))
          empty-s ; s
          (lambda (s version min-jump) s) ; k
          1 ; version
          0 ; min-jump
          ))))))

(define take
  (lambda (n f)
    (if (and n (zero? n))
      '()
      (case-inf (f)
        ((target mode) '())
        ((f) (take n f))
        ((a) a)
        ((a f)
         (cons (car a)
           (take (and n (- n 1)) f)))))))

(define ==
  (lambda (u v)
    (lambdag@ (s k version min-jump)
      (let-values ([(s s-f) (unify u v s version min-jump)])
        (if s
          (k s version min-jump)
          (failure s-f #f))))))

(define succeed (== #f #f))
(define fail (== #f #t))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s k version min-jump)
       (inc
         (let ((x (var 'x)) ...)
           ((bind* g0 g ...) s k version min-jump)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s k version min-jump)
       (inc
         (mplus*
           version
           ((bind* g0 g ...) s k (+ 1 version) min-jump)
           ((bind* g1 g^ ...) s k (+ 1 version) min-jump) ...))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (bind* g ...)))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ version e) e)
    ((_ version e0 e ...)
     (mplus
       version
       e0
       (lambdaf@ () (mplus* version e ...))))))

(define (conj g1 g2)
  (lambdag@ (s k top-version top-min-jump)
    (conj-top
      (g1
        s
        (lambda (s bottom-version bottom-min-jump)
          (let-values
            ([(version min-jump)
              ; check if the substitution has been extended in this
              ; subtree.
              (if (not (= top-version bottom-version))
                ; if so, go to new version on exit and set
                ; minimum jump to version established in this subtree.
                (values (+ 1 bottom-version)
                        bottom-version)
                ; if we didn't extend the substitution, leave the
                ; version and min-jump alone to allow jumping past the subtree
                (values bottom-version bottom-min-jump))])
            (conj-bottom
              (g2 s k version min-jump)
              bottom-version ; TODO: This might be the bug... Why is it this version? Why not version from let?
              top-version)))
        top-version
        top-min-jump)
      top-version)))

; Disjunction with two live branches
(define mplus
  (lambda (version a-inf f)
    (case-inf a-inf
      ((target mode)
       (if mode
         (failure target mode)
         (mplus-single (f) target)))
      ((f^) (inc (mplus version (f) f^)))
      ((a) (choice a (inc (mplus-succeeded (f) version))))
      ((a f^) (choice a (inc (mplus version (f) f^)))))))

(define mplus-succeeded
  (lambda (a-inf original-version)
    (case-inf a-inf
      ((target mode)
       (if mode
         (error 'btsucc "tried to destructively backtrack where we've already succeeded on one branch")
         (failure (max target original-version) #f)))
      ((f^) (inc (mplus-succeeded (f^) original-version)))
      ((a) a)
      ((a f^) (choice a (inc (mplus-succeeded (f^) original-version)))))
    ))

; Disjunction where one branch failed
(define mplus-single
  (lambda (a-inf other-target)
    (case-inf a-inf
      ((target mode)
       (if mode
         (failure target mode)
         (failure (max target other-target) #f)))
      ((f^) (inc (mplus-single (f^) other-target)))
      ((a) a)
      ((a f^) (choice a (inc (mplus-single (f^) other-target)))))))

(define conj-top
  (lambda (a-inf version)
    (case-inf a-inf
      ((target mode)
       (cond
         ;[(> target version) (error 'here "shouldn't be here")]
         ; backjump complete; backtracking by single version from here. TODO: should this be an inequality instead?
         [(>= target version) (failure (- version 1) #f)]
         ; switch back to combining mode. TODO: should this be a inequality instead?
         [(eqv? mode version) (failure target #f)]
         ; still destructively backtracking; fail.
         [else (failure target mode)]))
      ((f^) (inc (conj-top (f^) version)))
      ((a) a)
      ((a f^) (choice a (inc (conj-top (f^) version)))))))

(define conj-bottom
  (lambda (a-inf version top-version)
    (case-inf a-inf
      ((target mode)
       (cond
         ;[(> target version) (error 'here "shouldn't be here")]
         ; backjump complete; backtracking by single version from here. TODO: should this be an inequality instead?
         [(>= target version) (failure (- version 1) #f)]
         ; if not in destructive, switch to it
         [else (failure target (or mode top-version))]))
      ((f^) (inc (conj-bottom (f^) version top-version)))
      ((a) a)
      ((a f^) (choice a (inc (conj-bottom (f^) version top-version)))))))
