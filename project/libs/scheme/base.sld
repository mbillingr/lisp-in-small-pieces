(define-library (scheme base)
    (export ; ... <= => > >= _
            * + - / < =
            and append apply assoc assq assv
            binary-port? begin boolean?
            car cdr caar cadr cdar cddr
            call/cc call-with-current-continuation
            call-with-port
            case case-lambda
            char-ready?
            close-port close-input-port close-output-port
            current-error-port current-input-port current-output-port
            cond cons
            define define-record-type define-syntax
            dynamic-wind
            eof-object eof-object?
            eq? equal? eqv?
            error error-object? error-object-message error-object-irritants
            flush-output-port
            get-output-bytevector get-output-string
            if
            input-port? input-port-open?
            lambda length list
            let let-syntax
            letrec-syntax
            make-parameter
            member memq memv
            newline null?
            open-input-bytevector open-input-string
            open-output-bytevector open-output-string
            or
            output-port? output-port-open?
            pair? parameterize peek-char peek-u8 port? procedure?
            quasiquote quote
            raise raise-continuable
            read read-bytevector read-bytevector! read-char read-line read-string read-u8
            set! set-car! set-cdr!
            textual-port?
            u8-ready?
            vector vector? vector-ref
            with-exception-handler
            write-bytevector write-char write-string write-u8
            zero?)
    (import (sunny core)
            (sunny case-lambda)
            (sunny conditionals)
            (sunny dynwind)
            (sunny exception)
            (sunny lists)
            (sunny parameter)
            (sunny ports)
            (sunny quasiquote)
            (sunny record))
    (begin
      (define call-with-current-continuation call/cc)

      (define (zero? x)
        (= x 0))))
