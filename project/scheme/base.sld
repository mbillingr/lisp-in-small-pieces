(define-library (scheme base)
    (export ; ... <= => > >= _
            * + - / < =
            apply
            begin boolean?
            car cdr caar cadr cdar cddr
            call-with-current-continuation call/cc
            case case-lambda
            current-error-port current-input-port current-output-port
            cond cons
            define define-syntax
            display
            dynamic-wind
            eq? equal? eqv?
            error
            if
            lambda length list
            let let-syntax
            letrec-syntax
            make-parameter
            member memq memv
            newline null?
            pair? parameterize
            quote
            set! set-car! set-cdr!
            vector vector-ref)
    (import (sunny core)
            (sunny case-lambda)
            (sunny conditionals)
            (sunny dynwind)
            (sunny lists)
            (sunny parameter)
            (sunny ports))
    (begin
      (define = eqv?)
      (define call-with-current-continuation call/cc)))
