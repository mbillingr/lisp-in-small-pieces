(define-library (sunny exception)
    (export error error-object? error-object-message error-object-irritants
            raise raise-continuable with-exception-handler)
    (import (rename (sunny core)
                    (error sys-error))
            (sunny binding)
            (sunny conditionals)
            (sunny lists)
            (sunny dynwind))
    (begin
      (define <continuable> (vector '<continuable>))
      (define <error-object> (vector '<error-object>))

      (define initial-exception-handler sys-error)

      (define (current-exception-handler err)
        (initial-exception-handler
          (if (continuable? err)
              (continuable-err err)
              err)))


      (define (raise obj)
        (current-exception-handler obj))

      (define (raise-continuable obj)
        (current-exception-handler (cons <continuable> obj)))

      (define (continuable? err)
        (and (pair? err)
             (eq? (car err) <continuable>)))

      (define (continuable-err err)
        (cdr err))

      (define (with-exception-handler handler thunk)
        (let* ((old-handler current-exception-handler)
               (new-handler (lambda (err)
                              (with-handler old-handler
                                (lambda ()
                                  (cond ((continuable? err)
                                         (handler (continuable-err err)))
                                        (else
                                          (handler err)
                                          (raise "handler returned"))))))))
          (with-handler new-handler thunk)))

      (define (with-handler handler thunk)
        (let ((old-handler current-exception-handler))
          (dynamic-wind
            (lambda () (set! current-exception-handler handler))
            thunk
            (lambda () (set! current-exception-handler old-handler)))))


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
