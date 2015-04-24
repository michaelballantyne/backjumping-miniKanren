(define-syntax test-check
  (syntax-rules ()
                ((_ title tested-expression expected-result)
                 (begin
                   (printf "Testing ~s\n" title)
                   (let* ((expected expected-result)
                          (produced tested-expression))
                     (or (equal? expected produced)
                         (begin
                           (printf
                             "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                             'tested-expression expected produced)
                           #;(error 'bad "test failed"))))))))

(define (lset=? l1 l2)
  (or (equal? l1 l2)
      (and (subset l1 l2) (subset l2 l1))))

(define (subset l1 l2)
  (or
    (andmap
      (lambda (x)
        (member x l2))
      l1)))


(define-syntax test-check-no-order
  (syntax-rules ()
                ((_ title tested-expression expected-result)
                 (begin
                   (printf "Testing ~s\n" title)
                   (let* ((expected expected-result)
                          (produced tested-expression))
                     (or (lset=? expected produced)
                         (begin
                           (printf
                             "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                             'tested-expression expected produced)
                           #;(error 'bad "test failed"))))))))


