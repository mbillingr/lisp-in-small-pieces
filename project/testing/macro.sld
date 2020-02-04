(define-library (testing lib)
    (export delay force or)
    (import (scheme base))
    (begin
        (define-syntax delay
            (syntax-rules ()
                ((delay x) (lambda () x))))

        (define-syntax force
            (syntax-rules ()
                ((force x) (x))))

        (define-syntax or
            (syntax-rules ()
                ((or a b) (let ((temp a)) (if temp temp b)))
            )
        )))
