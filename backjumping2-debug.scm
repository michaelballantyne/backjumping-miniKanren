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

(define varctr 0)

(define-syntax var
  (syntax-rules ()
    ((_ x) (begin
             (set! varctr (+ 1 varctr))
             (vector varctr x)))))

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


#;(define occurs-check
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

(define-syntax run-inc
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (begin
       (set! count 0)
       (let ([t (current-time)])
         (take-inc n t t 1
                 (lambdaf@ ()
                           ((fresh (x) g0 g ...
                              (lambdag@ (s k version min-jump destructive-top)
                                        (cons (reify x s) '())))
                            empty-s ; s
                            (lambda (s version min-jump destructive-top) s) ; k
                            0 ; version
                            0 ; min-jump
                            0 ; destructive-top (oldest version a jump can destroy)
                            ))))))))

(define-syntax run-inc*
  (syntax-rules ()
    ((_ (x) g ...) (run-inc #f (x) g ...))))

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
         ((target mode) (begin
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

(define last-failure #f)

(define ==
  (lambda (u v)
    (lambdag@ (s-in k version min-jump destructive-top)
      (begin
        (let-values ([(s s-f) (unify u v s-in version min-jump)])
                    (set! count (+ 1 count))
                    (if s
                      (k s version min-jump destructive-top)
                      (begin
                        (set! last-failure `(failing-unification
                                              (numbers ,version ,min-jump ,destructive-top)
                                              (u ,u)
                                              (v ,v)
                                              (failure ,s-f ,destructive-top)
                                              (s ,s-in)))
                        (failure s-f destructive-top))))))))

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
           s
           k
           version
           min-jump
           destructive-top
           (bind* g0 g ...)
           (bind* g1 g^ ...) ...))))))


(define-syntax bind*
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (bind* g ...)))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ s k version min-jump destructive-top e) (e s k version min-jump destructive-top))
    ((_ s k version min-jump destructive-top e0 e ...)
     (mplus
       (e0 s k (+ 1 version) min-jump (+ 1 version))
       (lambdaf@ () (mplus* s k (+ 1 version) min-jump (+ 1 version) e ...))
       version
       min-jump
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

            ; (> bottom-version top-version)
            ; (> bottom-destructive-top top-version)
            (if (> bottom-destructive-top top-version)
              ; if so, go to new version on exit and set
              ; minimum jump to version established in this subtree.
              (values (+ 1 bottom-version)
                      bottom-version)
              ; if we didn't extend the substitution, leave the
              ; version and min-jump alone to allow jumping past the subtree
              (values bottom-version bottom-min-jump))])
          (conj-bottom (g2 s k version min-jump top-destructive-top) top-version top-destructive-top)))
      top-version
      top-min-jump
      top-destructive-top)))

; Disjunction with two live branches
(define mplus
  (lambda (a-inf f version min-jump destructive-top)
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
         (mplus-failure (f) version min-jump destructive-top (min target version))))
      ((f^) (inc (mplus (f) f^ version min-jump destructive-top)))
      ((a) (choice a (inc (mplus-success (f) version min-jump destructive-top version a))))
      ((a f^) (choice a (inc (mplus-success2 (f) f^ version min-jump destructive-top a)))))))

(define the-success #f)

(define mplus-success2
  (lambda (a-inf f version min-jump destructive-top a)
    (case-inf a-inf
      ((target mode)
       (if (and (<= target version) (<= mode version))
         ; Backjumping beyond here and have the mode to do it destructively.
         ; Propagate failure and if we have a more powerful destructive-top
         ; use that as new mode.
         (begin
           (set! the-success a)
           (display "\n")
           (pretty-print (list version min-jump destructive-top other-target target mode a))
           (display "\n")
           (display "\n")
           (pretty-print last-failure)
           (display "\n")
           (raise
             "destructive failure after success"))

         ; Otherwise backjumping stalls here. Resume when we know what the
         ; other branch does. If the stalled jump wouldn't reach past here
         ; we'll resume the jump with this node's version.
         (mplus-success (f) version min-jump destructive-top (min target version) a)))
      ((f^) (inc (mplus-success2 (f) f^ version min-jump destructive-top a)))
      ((a) (choice a (inc (mplus-success (f) version min-jump destructive-top version a))))
      ((a f^) (choice a (inc (mplus-success2 (f) f^ version min-jump destructive-top a)))))))

; Disjunction where one branch succeeded finitely or failed
(define mplus-failure
  (lambda (a-inf version min-jump destructive-top other-target)
    (case-inf a-inf
      ((target mode)
       (if (and (<= target version) (<= mode version))
         ; Backjumping beyond here and have the mode to do it destructively.
         ; Propagate failure and if we have a more powerful destructive-top
         ; use that as new mode.
         (failure target (min mode destructive-top)) ; TODO: previously had (min mode destructive-top) but that was apparently wrong. I don't understand why. Maybe it was right and something else is broken.

         ; Otherwise combine the failure with the previous. Ignore the mode
         ; of the triggering failure because either it wasn't powerful enough
         ; to jump past here or the jump ended prior to this point and we're
         ; on a new jump.
         (failure (max target other-target) destructive-top)))
      ((f^) (inc (mplus-failure (f^) version min-jump destructive-top other-target)))
      ((a) a)
      ((a f^) (choice a (inc (mplus-failure (f^) version min-jump destructive-top other-target)))))))


(define mplus-success
  (lambda (a-inf version min-jump destructive-top other-target a)
    (case-inf a-inf
      ((target mode)
       (if (and (<= target version) (<= mode version))
         ; Backjumping beyond here and have the mode to do it destructively.
         ; Propagate failure and if we have a more powerful destructive-top
         ; use that as new mode.
         (begin
           (set! the-success a)
           (display "\n")
           (pretty-print (list version min-jump destructive-top other-target target mode a))
           (display "\n")
           (display "\n")
           (pretty-print last-failure)
           (display "\n")
           (raise
             "destructive failure after success"))

         ; Otherwise combine the failure with the previous. Ignore the mode
         ; of the triggering failure because either it wasn't powerful enough
         ; to jump past here or the jump ended prior to this point and we're
         ; on a new jump.
         (failure (max target other-target) destructive-top)))
      ((f^) (inc (mplus-success (f^) version min-jump destructive-top other-target a)))
      ((a) a)
      ((a f^) (choice a (inc (mplus-success (f^) version min-jump destructive-top other-target a)))))))


(define conj-bottom
  (lambda (a-inf top-version top-destructive-top)
    (case-inf a-inf
      ((target mode)
       (failure target mode))
      ((f^) (inc (conj-bottom (f^) top-version top-destructive-top)))
      ((a) a)
      ((a f^) (choice a (inc (conj-bottom (f^) top-version top-destructive-top)))))))
