(set! fib
      (lambda (n)
        (if (< n 2)
            1
            (+ (fib (- n 1) (- n 2))))))

(fib 10)
