(define-library (sunny exception)
    (export error
            error-object? error-object-message error-object-irritants
            raise raise-continuable with-exception-handler)
    (import (sunny core)
            (sunny dynwind))
    (begin
      (define <error-object> (vector '<error-object>))

      (define (with-exception-handler handler thunk)
        (dynamic-wind
          (lambda () (push-exception-handler handler))
          thunk
          (lambda () (pop-exception-handler))))

      (define (error msg . obj*)
        (raise (make-error-object msg obj*)))

      (define (make-error-object message irritants)
        (cons <error-object> (cons message irritants)))

      (define (error-object? obj)
        (and (pair? obj)
             (eq? (car obj) <error-object>)))

      (define (error-object-message obj)
        (cadr obj))

      (define (error-object-irritants obj)
        (cddr obj))

      (define (tests)
        (define (test-continuable)
          (with-exception-handler
            (lambda (err) (display "E: ") (display err) (newline) 0)
            (lambda ()
              (with-exception-handler
                (lambda (f) (display "F: ") (display f) (newline) 1)
                (lambda ()
                  (with-exception-handler
                    (lambda (f) (display "G: ") (display f) (newline) 2)
                    (lambda () (display (raise-continuable 42)) (display "more"))))))))

        (define (test-fatal)
          (with-exception-handler
            (lambda (err) (display "E: ") (display err) (newline) 0)
            (lambda ()
              (with-exception-handler
                (lambda (f) (display "F: ") (display f) (newline) 1)
                (lambda ()
                  (with-exception-handler
                    (lambda (f) (display "G: ") (display f) (newline) 2)
                    (lambda () (display (raise 42)) (display "more")))))))))))
