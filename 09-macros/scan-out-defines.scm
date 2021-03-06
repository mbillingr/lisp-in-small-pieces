
(define (scan-out-defines body)
  (define (initializations exprs)
    (cond ((null? exprs)
           '())
          ((definition? (car exprs))
           (cons (list (definition-variable (car exprs))
                       '*unassigned*)
                 (initializations (cdr exprs))))
          (else (initializations (cdr exprs)))))
  (define (transform exprs)
    (cond ((null? exprs)
           '())
          ((definition? (car exprs))
           (cons `(set! ,(definition-variable (car exprs))
                        ,(definition-value (car exprs)))
                 (transform (cdr exprs))))
          (else (cons (car exprs)
                      (transform (cdr exprs))))))
  (let ((init (initializations body)))
    (if (null? init)
        body
        `((let ,init . ,(transform body))))))

(define (definition? expr)
  (and (pair? expr)
       (eq? (car expr) 'define)))

(define (definition-variable expr)
  (if (pair? (cadr expr))
      (caadr expr)
      (cadr expr)))

(define (definition-value expr)
  (if (pair? (cadr expr))
      `(lambda ,(cdadr expr) . ,(cddr expr))
      (caddr expr)))
