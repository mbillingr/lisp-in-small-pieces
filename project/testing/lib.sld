(define-library (testing lib)
    (export fib foo find-negative)
    (import (sunny base))
    (begin
        (define (foo x) (bar x))
        (define (bar x) x)

        (define (find-negative seq)
            (define (search exit)
                (display "searching ")
                (display seq)
                (newline)
                (if (pair? seq)
                    (begin
                        (if (< (car seq) 0)
                            (exit (car seq)))
                        (set! seq (cdr seq))
                        (search exit))
                    #f))
            (call/cc search))

        (define (fib n)
          (if (< n 2)
              1
              (+ (fib (- n 1)) (fib (- n 2)))))))
