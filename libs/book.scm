
(define-library (libs book)

  (export a-true-value
          atom?
          empty-begin
          extend
          lookup
          the-false-value
          update!
          wrong)

  (import (builtin core)
          (libs utils))

  (begin
    (define wrong error)
    (define empty-begin 'empty-begin)
    (define the-false-value (cons "boolean" "false"))
    (define a-true-value (cons "boolean" "true"))

    (define (atom? exp) (not (pair? exp)))

    (define (lookup id env)
      (if (pair? env)
          (if (eq? (caar env) id)
              (cdar env)
              (lookup id (cdr env)))
          (wrong "No such binding" id)))

    (define (update! id env value)
      (if (pair? env)
          (if (eq? (caar env) id)
              (begin (set-cdr! (car env) value)
                     value)
              (update! id (cdr env) value))
          (wrong "No such binding" id)))

    (define (extend env variables values)
      (cond ((pair? variables)
             (if (pair? values)
                 (cons (cons (car variables) (car values))
                       (extend env (cdr variables) (cdr values)))
                 (wrong "Too few values")))
            ((null? variables)
             (if (null? values)
                 env
                 (wrong "Too many values")))
            ((symbol? variables) (cons (cons variables values) env))))))
