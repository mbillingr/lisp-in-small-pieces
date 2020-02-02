(define-library (testing lib)
    (export foo)
    (begin
        (define (foo . x) x)))
