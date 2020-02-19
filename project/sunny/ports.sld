(define-library (synny ports)
    (export call-with-port
            close-port
            current-error-port current-input-port current-output-port
            display
            eof-object?
            flush-output-port
            input-port?
            newline
            output-port?
            port?
            read-char)
    (import (sunny core)
            (prefix (sunny ports-core) spc-)
            (only (sunny ports-core)
                  close-port
                  eof-object?
                  input-port?
                  output-port?
                  port?)
            (sunny dynwind)
            (sunny parameter))
    (begin
      (define current-input-port (make-parameter (spc-open-standard-input)))
      (define current-output-port (make-parameter (spc-open-standard-output)))
      (define current-error-port (make-parameter (spc-open-standard-error)))

      ;; R7RS requires that the port is closed only if the function returns,
      ;; unless it is possible to prove the port will never again be used.
      ;; Ostensibly, the port will be closed when being garbage collected. In
      ;; our implementation this will not happen because Rust does not know
      ;; about the garbage collector...
      (define (call-with-port port proc)
        (dynamic-wind
          (lambda () #f)
          (lambda () (proc port))
          (lambda ()
            (if (output-port? port)
                (if (spc-port-open? port)
                    (display "flushing") (newline)
                    (spc-flush-output-port port)))))
        (display "closing") (newline)
        (close-port port))

      (define (newline . o)
        (if (null? o)
            (spc-newline (current-output-port))
            (spc-newline (car o))))

      (define (display obj . o)
        (if (null? o)
            (spc-display obj (current-output-port))
            (spc-newline obj (car o))))

      (define (read-char . o)
        (if (null? o)
            (spc-read-char (current-input-port))
            (spc-read-char (car o))))

      (define (flush-output-port . o)
        (if (null? o)
            (spc-flush-output-port (current-input-port))
            (spc-flush-output-port (car o))))))
