(define-library (testing lib)
    (export foo)
    (import (scheme base))
    (begin
        (define (foo x) (bar x))
        (define (bar x) x)))
