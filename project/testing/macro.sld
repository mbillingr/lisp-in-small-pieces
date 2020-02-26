(define-library (testing macro)
    (export delay force or mul
            (rename add add_many))
    (import (sunny core))
    (begin
        (define-syntax delay
            (syntax-rules ()
                ((delay x) (lambda () x))))

        (define-syntax force
            (syntax-rules ()
                ((force x) (x))))

        (define-syntax or
            (syntax-rules ()
                ((or) #f)
                ((or a) a)
                ((or a b) (let ((temp a)) (if temp temp b)))
                ((or a b ...) (let ((temp a)) (if temp temp (or b ...))))))

        (define-syntax add
            (syntax-rules ()
                ((add) 0)
                ((add z) z)
                ((add z1 z2 ...) (+ z1 (add z2 ...)))))

        (define-syntax mul
            (syntax-rules ()
                ((mul) 1)
                ((mul z) z)
                ((mul z1 z2 ...) (* z1 (mul z2 ...)))))))
