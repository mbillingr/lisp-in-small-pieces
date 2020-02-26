(define-library (sunny lists)
    (export append caar cadr cdar cddr length member memq memv)
    (import (sunny core)
            (sunny conditionals))
    (begin
      (define (append . lists)
        (define (append* lists)
          (cond ((null? lists)
                 '())
                ((null? (cdr lists))
                 (car lists))
                (else (append-one (car lists) (append* (cdr lists))))))
        (define (append-one list1 list2)
          (if (null? list1)
              list2
              (cons (car list1)
                    (append-one (cdr list1) list2))))
        (append* lists))

      (define (caar x) (car (car x)))
      (define (cadr x) (car (cdr x)))
      (define (cdar x) (cdr (car x)))
      (define (cddr x) (cdr (cdr x)))

      (define (length list)
        (define (scan rest len)
          (if (pair? rest)
              (scan (cdr rest) (+ 1 len))
              len))
        (scan list 0))

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
