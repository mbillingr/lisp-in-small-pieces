(define-library (synny ports)
    (export current-error-port current-input-port current-output-port
            display
            newline
            read-char)
    (import (sunny core)
            (prefix (sunny ports-core) spc-)
            (sunny parameter))
    (begin
      (define current-input-port (make-parameter (spc-open-standard-input)))
      (define current-output-port (make-parameter (spc-open-standard-output)))
      (define current-error-port (make-parameter (spc-open-standard-error)))

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
            (spc-read-char (car o))))))
