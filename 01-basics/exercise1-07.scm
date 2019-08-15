; interpreter written in continuation passing style so it supports call/cc
;
; example 1 for testing:
;    (set! foo (lambda (return) (return 2) 3))
;    (foo (lambda (x) x))  ; returns 3
;    (call/cc foo)  ; returns 2

(import (builtin core)
        (libs utils))

(define (evaluate e env continuation)
  (if (atom? e)
      (cond ((symbol? e) (continuation (lookup e env)))
            ((or (number? e) (string? e) (char? e) (boolean? e))
             (continuation e))
            (else (wrong "Cannot evaluate" e)))

      (case (car e)
        ((quote)  (continuation (cadr e)))
        ((if)     (evaluate (cadr e) env
                    (lambda (cond)
                      (if (not (eq? cond the-false-value))
                          (evaluate (caddr e) env continuation)
                          (evaluate (cadddr e) env continuation)))))
        ((begin)  (eprogn (cdr e) env continuation))
        ((set!)   (evaluate (caddr e) env
                    (lambda (value)
                      (update! (cadr e) env value)
                      (continuation value))))
        ((lambda) (make-function (cadr e) (cddr e) env continuation))
        (else     (evaluate (car e) env
                    (lambda (fn)
                      (evlis (cdr e) env
                        (lambda (args)
                          (invoke fn args continuation)))))))))

(define (atom? exp) (not (pair? exp)))

(define wrong error)

(define empty-begin 'empty-begin)
(define the-false-value (cons "boolean" "false"))
(define a-true-value (cons "boolean" "true"))

(define (eprogn exps env continuation)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (evaluate (car exps) env
            (lambda (_) (eprogn (cdr exps) env continuation)))
          (evaluate (car exps) env continuation))
      (continuation empty-begin)))

(define (evlis exps env continuation)
  (if (pair? exps)
      (evaluate (car exps) env
        (lambda (argument)
          (evlis (cdr exps) env
            (lambda (restarg)
              (continuation (cons argument restarg))))))
      (continuation '())))

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

(define (invoke fn args continuation)
  (cond ((procedure? fn) (fn args continuation))
        ((continuation? fn) (apply (cdr fn) args))
        (else (wrong "Not a function" fn))))

(define (continuation? fn)
  (and (pair? fn) (eq? (car fn) 'continuation)))

(define (make-function variables body env continuation)
  (continuation
    (lambda (values cont)
      (eprogn body (extend env variables values) cont))))

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
    (lambda (values continuation)
      (if (= arity (length values))
          (continuation (apply value values))
          (wrong "Incorrect arity" (list name values))))))

(definitial 't a-true-value)
(definitial 'f the-false-value)
(definitial 'nil '())

(definitial 'foo)
(definitial 'bar)
(definitial 'fib)
(definitial 'fact)
(definitial 'list)

(definitial 'call/cc
  (lambda (f continuation)
    (invoke (car f) (list (cons 'continuation continuation)) continuation)))

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
  (define (toplevel value)
    (display value)
    (evaluate (read) env.global toplevel))
  (toplevel "Welcome to the CPS Interpreter"))

(chapter1-scheme)

;(set! fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
