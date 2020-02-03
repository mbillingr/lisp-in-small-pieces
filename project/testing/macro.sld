(define-library (testing lib)
    (export delay force)
    (begin
        (define-syntax delay
            (syntax-rules ()
                ((delay x) (lambda () x))))

        (define-syntax force
            (syntax-rules ()
                ((force x) (x))))))
