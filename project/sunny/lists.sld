(define-library (sunny lists)
    (export caar cadr cdar cddr member memq memv)
    (import (sunny core))
    (begin
      (define (caar x) (car (car x)))
      (define (cadr x) (car (cdr x)))
      (define (cdar x) (cdr (car x)))
      (define (cddr x) (cdr (cdr x)))

      (define (memq obj list)
        (if (pair? list)
            (if (eq? obj (car list))
                list
                (memq obj (cdr list)))
            #f))

      (define (memv obj list)
        (if (pair? list)
            (if (eqv? obj (car list))
                list
                (memv obj (cdr list)))
            #f))

      (define (member obj list . optional)
        (if (pair? optional)
            (member_ obj list (car optional))
            (member_ obj list equal?)))

      (define (member_ obj list compare?)
        (if (pair? list)
            (if (compare? obj (car list))
                list
                (member_ obj (cdr list) compare?))
            #f))))
