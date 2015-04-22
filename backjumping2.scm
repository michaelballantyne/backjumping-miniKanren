(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s k version min-jump destructive-top) e) (lambda (s k version min-jump destructive-top) e))))

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
        ((var? u) (ext-s-check u v version s (max u-f v-f)))
        ((var? v) (ext-s-check v u version s (max u-f v-f)))
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
      (else (values (ext-s x v version s jump) #f)))))

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


(define count 0)

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (begin
       (set! count 0)
       (take n
             (lambdaf@ ()
                       ((fresh (x) g0 g ...
                          (lambdag@ (s k version min-jump destructive-top)
                                    (cons (reify x s) '())))
                        empty-s ; s
                        (lambda (s version min-jump destructive-top) s) ; k
                        0 ; version
                        0 ; min-jump
                        0 ; destructive-top (oldest version a jump can destroy)
                        )))))))

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
    (lambdag@ (s k version min-jump destructive-top)
      (let-values ([(s s-f) (unify u v s version min-jump)])
        (set! count (+ 1 count))
        (if s
          (k s version min-jump destructive-top)
          (failure s-f destructive-top))))))

(define succeed (== #f #f))
(define fail (== #f #t))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s k version min-jump destructive-top)
       (inc
         (let ((x (var 'x)) ...)
           ((bind* g0 g ...) s k version min-jump destructive-top)))))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s k version min-jump destructive-top)
       (inc
         (mplus*
           version
           destructive-top
           ((bind* g0 g ...) s k (+ 1 version) min-jump (+ 1 version))
           ((bind* g1 g^ ...) s k (+ 1 version) min-jump (+ 1 version)) ...))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (bind* g ...)))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ version destructive-top e) e)
    ((_ version destructive-top e0 e ...)
     (mplus
       e0
       (lambdaf@ () (mplus* version destructive-top e ...))
       version
       destructive-top
       ))))

(define (conj g1 g2)
  (lambdag@ (s k top-version top-min-jump top-destructive-top)
    (g1
      s
      (lambda (s bottom-version bottom-min-jump bottom-destructive-top)
        (let-values
          ([(version min-jump)
            ; check if the substitution has been extended in this
            ; subtree.
            (if (and (not (= top-version bottom-version)) (= bottom-version bottom-destructive-top))
              ; if so, go to new version on exit and set
              ; minimum jump to version established in this subtree.
              (values (+ 1 bottom-version)
                      bottom-version)
              ; if we didn't extend the substitution, leave the
              ; version and min-jump alone to allow jumping past the subtree
              (values bottom-version bottom-min-jump))])
          (g2 s k version min-jump top-destructive-top)))
      top-version
      top-min-jump
      top-destructive-top)))

; Disjunction with two live branches
(define mplus
  (lambda (a-inf f version destructive-top)
    (case-inf a-inf
      ((target mode)
       (if (and (<= target version) (<= mode version))
         ; Backjumping beyond here and have the mode to do it destructively.
         ; Propagate failure and if we have a more powerful destructive-top
         ; use that as new mode.
         (failure target (min mode destructive-top))

         ; Otherwise backjumping stalls here. Resume when we know what the
         ; other branch does. If the stalled jump wouldn't reach past here
         ; we'll resume the jump with this node's version.
         (mplus-single (f) version (min target version) destructive-top)))
      ((f^) (inc (mplus (f) f^ version destructive-top)))
      ((a) (choice a (inc (mplus-single (f) version version destructive-top))))
      ((a f^) (choice a (inc (mplus (f) f^ version destructive-top)))))))

; Disjunction where one branch succeeded finitely or failed
(define mplus-single
  (lambda (a-inf version other-target destructive-top)
    (case-inf a-inf
      ((target mode)
       (if (and (<= target version) (<= mode version))
         ; Backjumping beyond here and have the mode to do it destructively.
         ; Propagate failure and if we have a more powerful destructive-top
         ; use that as new mode.
         (failure target (min mode destructive-top))

         ; Otherwise combine the failure with the previous. Ignore the mode
         ; of the triggering failure because either it wasn't powerful enough
         ; to jump past here or the jump ended prior to this point and we're
         ; on a new jump.
         (failure (max target other-target) destructive-top)))
      ((f^) (inc (mplus-single (f^) version other-target destructive-top)))
      ((a) a)
      ((a f^) (choice a (inc (mplus-single (f^) version other-target destructive-top)))))))

