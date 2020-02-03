(define-library (testing lib)
    (export foo)
    (begin
        (define (foo x) (bar x))
        (define (bar x) x)))
