(define-library (sunny binding)
    (export let let*)
    (import (sunny core))
    (begin
      (define-syntax let
        (syntax-rules ()
          ((let ((name val) ...) body1 body2 ...)
           ((lambda (name ...) body1 body2 ...)
            val ...))
          ((let tag ((name val) ...) body1 body2 ...)
           ; TODO: replace with letrec as in R7RS
           (let ((tag #f))
             (set! tag (lambda (name ...) body1 body2 ...))
             (tag val ...)))))

      (define-syntax let*
        (syntax-rules ()
          ((let* () body1 body2 ...)
           (let () body1 body2 ...))
          ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
           (let ((name1 val1))
             (let* ((name2 val2) ...)
               body1 body2 ...)))))))
