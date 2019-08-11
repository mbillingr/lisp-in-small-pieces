(import (builtin core)
        (libs utils))

(define (evaluate e env)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e)) e)
            (else (wrong "Cannot evaluate" e)))

      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (evaluate (cadr e) env)
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env)))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env)
                          env)))))

(define (atom? exp) (not (pair? exp)))

(define wrong error)

(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))

(define empty-begin 'empty-begin)

(define (evlis exps env)
  (if (pair? exps)
      (let ((argument (evaluate (car exps) env)))
        (cons argument (evlis (cdr exps) env)))
      '()))

(define (lookup id env)
  (car (getprop id 'apval)))

(define (update! id env value)
  (set-car! (getprop id 'apval) value))

(define (invoke fn args env)
  (if (procedure? fn)
      (fn args env)
      (wrong "Not a function" fn)))

(define (make-function variables body env)
  (lambda (values current.env)
    (map (lambda (var val) (push-env var val))
         variables
         values)
    (let ((result (eprogn body current.env)))
      (for-each (lambda (var) (pop-env var))
                variables)
      result)))

(define (push-env var val)
  (putprop var 'apval (cons val (getprop var 'apval))))

(define (pop-env var)
  (let ((top (getprop var 'apval)))
    (putprop var 'apval (cdr top))
    (car top)))

(define getprop get)
(define putprop put)

(define env.init '())

(define env.global env.init)

(define (definitial name . value)
  (if (null? value)
      (begin (push-env name 'void)
             name)
      (begin (push-env name (car value))
             name)))

(define (defprimitive name value arity)
  (definitial name
    (lambda (values env)
      (if (= arity (length values))
          (apply value values)
          (wrong "Incorrect arity" (list name values))))))

(definitial 't #t)
(definitial 'f 'the-false-value)
(definitial 'nil '())

(definitial 'foo)
(definitial 'bar)
(definitial 'fib)
(definitial 'fact)

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
(defprimitive '< < 2)

(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))
