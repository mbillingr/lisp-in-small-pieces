(define-library (scheme base)
    (export case cond member memq memv)
    (import (sunny base))
    (begin

      (define-syntax cond
        (syntax-rules (else =>)
          ((cond (else result1 result2 ...))
           (begin result1 result2 ...))
          ((cond (test => result))
           (let ((temp test))
             (if temp (result temp))))
          ((cond (test => result) clause1 clause2 ...)
           (let ((temp test))
             (if temp
                 (result temp)
                 (cond clause1 clause2 ...))))
          ((cond (test)) test)
          ((cond (test) clause1 clause2 ...)
           (let ((temp test))
             (if temp
                 temp
                 (cond clause1 clause2 ...))))
          ((cond (test result1 result2 ...))
           (if test (begin result1 result2 ...)))
          ((cond (test result1 result2 ...)
                 clause1 clause2 ...)
           (if test
               (begin result1 result2 ...)
               (cond clause1 clause2 ...)))))

      (define-syntax case
        (syntax-rules (else =>)
          ((case (key ...)
             clauses ...)
           (let ((atom-key (key ...)))
             (case atom-key clauses ...)))
          ((case key
             (else => result))
           (result key))
          ((case key
             (else result1 result2 ...))
           (begin result1 result2 ...))
          ((case key
             ((atoms ...) result1 result2 ...))
           (if (memv key '(atoms ...))
               (begin result1 result2 ...)))
          ((case key
             ((atoms ...) => result))
           (if (memv key '(atoms ...))
               (result key)))
          ((case key
             ((atoms ...) => result)
             clause clauses ...)
           (if (memv key '(atoms ...))
               (result key)
               (case key clause clauses ...)))
          ((case key
             ((atoms ...) result1 result2 ...)
             clause clauses ...)
           (if (memv key '(atoms ...))
               (begin result1 result2 ...)
               (case key clause clauses ...)))))

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
