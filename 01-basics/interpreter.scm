(import (builtin core)
        (libs utils))

(define (evaluate e env)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e)) e)
            (else (wrong "Cannot evaluate" e)))

      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (not (eq? (evaluate (cadr e) env) the-false-value))
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env)))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env))))))

(define (atom? exp) (not (pair? exp)))

(define wrong error)

(define empty-begin 'empty-begin)
(define the-false-value (cons "boolean" "false"))
(define a-true-value (cons "boolean" "true"))

(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))

(define (evlis exps env)
  (if (pair? exps)
      (let ((argument (evaluate (car exps) env)))
        (cons argument (evlis (cdr exps) env)))
      '()))

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
        ((symbol? variables) (cons (cons variables values) env))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))

(define env.init '())

(define env.global env.init)

(define (definitial name . value)
  (if (null? value)
      (begin (set! env.global (cons (cons name 'void) env.global))
             name)
      (begin (set! env.global (cons (cons name (car value)) env.global))
             name)))

(define (defprimitive name value arity)
  (definitial name
    (lambda (values)
      (if (= arity (length values))
          (apply value values)
          (wrong "Incorrect arity" (list name values))))))

(definitial 't a-true-value)
(definitial 'f the-false-value)
(definitial 'nil '())

(definitial 'foo)
(definitial 'bar)
(definitial 'fib)
(definitial 'fact)
(definitial 'list)

(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defprimitive 'set-car! set-car! 2)
(defprimitive 'set-cdr! set-cdr! 2)
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defprimitive '* * 2)
(defprimitive '/ / 2)
(defprimitive 'eq? eq? 2)
(defprimitive '<
              (lambda (a b)
                (if (< a b)
                    a-true-value
                    the-false-value))
              2)

(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))

(chapter1-scheme)

;(set! fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
