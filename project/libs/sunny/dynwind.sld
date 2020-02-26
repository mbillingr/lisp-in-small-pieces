(define-library (synny dynwind)
    (export call/cc call/ep dynamic-wind)
    (import (sunny core)
            (only (sunny conditionals) cond))
    (begin
        (define make-point vector)
        (define (point-depth point) (vector-ref point 0))
        (define (point-before point) (vector-ref point 1))
        (define (point-after point) (vector-ref point 2))
        (define (point-parent point) (vector-ref point 3))

        (define root-point
          (make-point 0
                      (lambda () (error "winding in to root!"))
                      (lambda () (error "winding out of root!"))
                      #f))

        (define get-current-point #f)
        (define set-current-point! #f)

        (let ((dk root-point))
          (set! get-current-point (lambda () dk))
          (set! set-current-point! (lambda (point) (set! dk point))))

        (define (dynamic-wind before thunk after)
          (before)
          (let ((here (get-current-point)))
            (set-current-point!
              (make-point (+ (point-depth here) 1)
                          before
                          after
                          here))
            (let ((result (thunk)))
              (set-current-point! here)
              (after)
              result)))

        (define (travel-to-point! here target)
          (cond ((eq? here target) 'done)
                ((< (point-depth here) (point-depth target))
                 (travel-to-point! here (point-parent target))
                 ((point-before target)))
                (else
                 ((point-after here))
                 (travel-to-point! (point-parent here) target))))

        (define (continuation->procedure cont point)
          (lambda (res)
            (travel-to-point! (get-current-point) point)
            (set-current-point! point)
            (cont res)))

        (define (call/cc proc)
          (let/cc cont
            (proc (continuation->procedure cont (get-current-point)))))

        (define (call/ep proc)
          (let/ep cont
            (proc (continuation->procedure cont (get-current-point)))))

        (define (dummy)
          (define runner #f)
          (dynamic-wind (lambda () (display "before") (newline))
                        (lambda () (display "thunk-in") (newline)
                                   (call/cc (lambda (cnt) (set! runner cnt)))
                                   (display "thunk-out") (newline))
                        (lambda () (display "after") (newline))))

        (define (dummy2)
          (call/ep
            (lambda (exit)
              (dynamic-wind (lambda () (display "before") (newline))
                            (lambda () (display "thunk-in") (newline)
                                       (exit 42)
                                       (display "thunk-out") (newline))
                            (lambda () (display "after") (newline))))))))
