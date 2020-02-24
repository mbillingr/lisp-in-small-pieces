(define-library (sunny parameter)
    (export make-parameter parameterize)
    (import (sunny core)
            (sunny dynwind)
            (only (sunny conditionals) cond)
            (only (sunny binding) let*)
            (only (sunny lists) cadr))
    (begin

        (define <param-set!> (vector 'param-set!))
        (define <param-convert> (vector 'param-convert))

        (define (make-parameter init . o)
          (let* ((converter
                   (if (pair? o) (car o) (lambda (x) x)))
                 (value (converter init)))
            (lambda args
              (cond
                ((null? args) value)
                ((eq? (car args) <param-set!>) (set! value (cadr args)))
                ((eq? (car args) <param-convert>) converter)
                (else error "bad parameter syntax")))))

        (define-syntax parameterize
          (syntax-rules ()
            ((parameterize ("step")
                           ((param value p old new) ...)
                           ()
                           body)
             (let ((p param) ...)
                (let ((old (p)) ...
                      (new ((p <param-convert>) value)) ...)
                  (dynamic-wind
                    (lambda () (p <param-set!> new) ...)
                    (lambda () . body)
                    (lambda () (p <param-set!> old) ...)))))
            ((parameterize ("step")
                           args
                           ((param value) . rest)
                           body)
             (parameterize ("step")
                           ((param value p old new) . args)
                           rest
                           body))
            ((parameterize ((param value) ...) . body)
             (parameterize ("step")
                           ()
                           ((param value) ...)
                           body))))))
