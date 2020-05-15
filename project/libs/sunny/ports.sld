(define-library (sunny ports)
    (export call-with-port
            call-with-input-file
            call-with-output-file
            input-port?
            output-port?
            textual-port?
            binary-port?
            port?
            input-port-open?
            output-port-open?
            current-input-port
            current-output-port
            current-error-port
            with-input-from-file
            with-output-to-file
            open-input-file
            open-binary-input-file
            open-output-file
            open-binary-output-file
            close-port
            close-input-port
            close-output-port
            open-input-string
            open-output-string
            get-output-string
            open-input-bytevector
            open-output-bytevector
            get-output-bytevector
            read
            read-char
            peek-char
            read-line
            eof-object?
            eof-object
            char-ready?
            read-string
            read-u8
            peek-u8
            u8-ready?
            read-bytevector
            read-bytevector!
            write
            write-shared
            write-simple
            display
            newline
            write-char
            write-string
            write-u8
            write-bytevector
            flush-output-port)
    (import (sunny core)
            (prefix (sunny ports-core) spc-)
            (only (sunny ports-core)
                  close-port
                  eof-object
                  eof-object?
                  get-output-bytevector
                  get-output-string
                  input-port?
                  open-input-file
                  open-output-file
                  open-input-bytevector
                  open-input-string
                  open-output-bytevector
                  open-output-string
                  output-port?
                  port?)
            (rename (only (sunny ports-core)
                          open-input-file
                          open-output-file
                          close-port)
                    (open-input-file open-binary-input-file)
                    (open-output-file open-binary-output-file)
                    (close-port close-input-port))
            (rename (only (sunny ports-core)
                          close-port)
                    (close-port close-output-port))
            (sunny case-lambda)
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
                    (spc-flush-output-port port)))))
        (close-port port))

      (define (call-with-input-file filename proc)
        (call-with-port (open-input-file filename) proc))

      (define (call-with-output-file filename proc)
        (call-with-port (open-output-file filename) proc))

      (define input-port-open? spc-port-open?)
      (define output-port-open? spc-port-open?)

      (define textual-port? port?)
      (define binary-port? port?)

      (define (with-input-from-file filename thunk)
        (let ((port open-input-file filename))
          (parameterize ((current-input-port port))
            (let ((result (thunk)))
              (close-port port)
              result))))

      (define (with-output-to-file filename thunk)
        (let ((port open-output-file filename))
          (parameterize ((current-output-port port))
            (let ((result (thunk)))
              (close-port port)
              result))))

      ; This function is difficult to implement because there is no equivalent
      ; non-blocking function availible on stdin.
      ; Chibi Scheme apparently chose to make this function blocking. (R7RS
      ; does not explicitly state it must be non-blocking, but a blocking
      ; char-ready? function does not seem very useful.)
      ; I'm not sure which is moro portable - making it blocking or always
      ; return #t and have (read-char) block unexpectedly...
      (define (char-ready? . o) #t)
      (define (u8-ready? . o) #t)

      (define-syntax optional-port
        (syntax-rules (input)
          ((_ input (name arg ...))
           (lambda (arg ... . o)
             (if (null? o)
                 (name arg ... (current-input-port))
                 (name arg ... (car o)))))
          ((_ output (name arg ...))
           (lambda (arg ... . o)
             (if (null? o)
                 (name arg ... (current-output-port))
                 (name arg ... (car o)))))))

      (define read (optional-port input (spc-read)))
      (define read-char (optional-port input (spc-read-char)))
      (define peek-char (optional-port input (spc-peek-char)))
      (define read-line (optional-port input (spc-read-line)))
      (define read-string (optional-port input (spc-read-string k)))
      (define read-u8 (optional-port input (spc-read-u8)))
      (define peek-u8 (optional-port input (spc-peek-u8)))
      (define read-bytevector (optional-port input (spc-read-bytevector k)))

      (define read-bytevector!
        (case-lambda
          ((bv) (read-bytevector! bv (current-input-port)))
          ((bv port) (read-bytevector! bv port 0))
          ((bv port start) (read-bytevector! bv port start (bytevector-length bv)))
          ((bv port start end) (spc-read-bytevector! bv port start end))))

      (define write (optional-port output (spc-write obj)))
      (define write-shared (optional-port output (spc-write-shared obj)))
      (define write-simple (optional-port output (spc-write-simple obj)))
      (define display (optional-port output (spc-display obj)))
      (define newline (optional-port output (spc-newline)))
      (define write-char (optional-port output (spc-write-char obj)))
      (define write-u8 (optional-port output (spc-write-u8 obj)))
      (define flush-output-port (optional-port output (spc-flush-output-port)))

      (define write-string
        (case-lambda
          ((str) (write-string str (current-input-port)))
          ((str port) (write-string str port 0))
          ((str port start) (write-string str port start (string-length str)))
          ((str port start end) (spc-write-string str port start end))))

      (define write-bytevector
        (case-lambda
          ((bv) (write-bytevector bv (current-input-port)))
          ((bv port) (write-bytevector bv port 0))
          ((bv port start) (write-bytevector bv port start (bytevector-length bv)))
          ((bv port start end) (spc-write-bytevector bv port start end))))))
