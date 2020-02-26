(define-library (synny binding)
    (export let*)
    (import (sunny core))
    (begin
      (define-syntax let*
        (syntax-rules ()
          ((let* () body1 body2 ...)
           (let () body1 body2 ...))
          ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...)
           (let ((name1 val1))
             (let* ((name2 val2) ...)
               body1 body2 ...)))))))
