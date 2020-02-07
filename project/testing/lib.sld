(define-library (testing lib)
    (export foo)
    (import (sunny base))
    (begin
        (define (foo x) (bar x))
        (define (bar x) x)))
