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
            even?
            exact expt
            floor flush-output-port
            get-output-bytevector get-output-string
            if
            input-port? input-port-open?
            lambda length list
            let let* let-syntax
            letrec-syntax
            make-parameter make-vector map
            member memq memv
            newline not null? number? number->string
            odd?
            open-input-bytevector open-input-string
            open-output-bytevector open-output-string
            or
            output-port? output-port-open?
            pair? parameterize peek-char peek-u8 port? procedure?
            quasiquote quote
            raise raise-continuable
            reverse
            read read-bytevector read-bytevector! read-char read-line read-string read-u8
            remainder
            round
            set! set-car! set-cdr!
            string->symbol string-append string-length string-ref string-set!
            textual-port?
            u8-ready?
            vector vector? vector-fill! vector-for-each vector-length vector-ref vector-set!
            with-exception-handler
            write-bytevector write-char write-string write-u8
            zero?)
    (import (sunny core)
            (sunny binding)
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
        (= x 0))

      (define remainder %)

      (define (even? x)
        (zero? (remainder x 2)))

      (define (odd? x)
        (= 1 (remainder x 2)))

      (define (not b)
        (eq? b #f))

      (define (min a b)
        (if (< a b) a b))

      ; simplistic definition of map, that takes only one list
      (define (map proc list)
        (if (null? list)
            '()
            (cons (proc (car list))
                  (map proc (cdr list)))))

      (define (fold op init list)
        (if (null? list)
            init
            (fold op
                  (op init (car list))
                  (cdr list))))

      (define (vector-for-each proc . vecs)
        (let* ((n (fold min
                        (vector-length (car vecs))
                        (map vector-length (cdr vecs)))))
          (let loop ((i 0))
            (if (< i n)
                (begin
                  (apply proc
                         (map (lambda (v) (vector-ref v i))
                              vecs))
                  (loop (+ i 1)))))))))
