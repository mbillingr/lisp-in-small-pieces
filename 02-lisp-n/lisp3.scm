(import (builtin core)
        (libs utils)
        (libs book))

(define (df.evaluate e env fenv denv)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (df.evaluate (cadr e) env fenv denv)
                      (df.evaluate (caddr e) env fenv denv)
                      (df.evaluate (cadddr e) env fenv denv)))
        ((begin)  (df.eprogn (cdr e) env fenv denv))
        ((set!)   (update! (cadr e)
                           env
                           (df.evaluate (caddr e) env fenv denv)))
        ;((lambda) (df.make-function (cadr e) (cddr e) env fenv))
        ((function)
         (cond ((symbol? (cadr e))
                (f.lookup (cadr e) fenv))
               ((and (pair? (cadr e)) (eq? (car (cadr e)) 'lambda))
                (df.make-function
                  (cadr (cadr e)) (cddr (cadr e)) env fenv))
               (else (wrong "Incorrect function" (cadr e)))))
        ((dynamic) (lookup (cadr e) denv))
        ((dynamic-set!)
         (update! (cadr e)
                  denv
                  (df.evaluate (caddr e) env fenv denv)))
        ((dynamic-let)
         (df.eprogn (cddr e)
                    env
                    fenv
                    (extend denv
                            (map car (cadr e))
                            (map (lambda (e) (df.evaluate e env fenv denv))
                                 (map cadr (cadr e))))))
        ;((flet)
        ; (f.eprogn (cddr e)
        ;           env
        ;           (extend fenv
        ;                   (map car (cadr e))
        ;                   (map (lambda (def)
        ;                          (f.make-function (cadr def) (cddr def)
        ;                                           env fenv))
        ;                        (cadr e)))))
        ;((labels)
        ; (let ((new-fenv (extend fenv
        ;                         (map car (cadr e))
        ;                         (map (lambda (def) 'void) (cadr e))))
        ;    (for-each (lambda (def)
        ;                (update! (car def)
        ;                         new-fenv
        ;                         (f.make-function (cadr def) (cddr def)
        ;                                          env new-fenv)))
        ;              (cadr e))
        ;    (f.eprogn (cddr e) env new-fenv)))
        (else     (df.evaluate-application (car e)
                                           (df.evlis (cdr e) env fenv denv)
                                           env
                                           fenv
                                           denv)))))

(define (df.evaluate-application fn args env fenv denv)
  (cond ((symbol? fn) ((f.lookup fn fenv) args denv))
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (df.eprogn (cddr fn)
                    (extend env (cadr fn) args)
                    fenv
                    denv))
        (else (wrong "Incorrect functional term" fn))))

(define (f.lookup id fenv)
  (if (pair? fenv)
      (if (eq? (caar fenv) id)
          (cdar fenv)
          (f.lookup id (cdr fenv)))
      (lambda (values)
        (wrong "No such functional binding" id))))

(define (df.evlis exps env fenv denv)
  (if (pair? exps)
      (cons (df.evaluate (car exps) env fenv denv)
            (df.evlis (cdr exps) env fenv denv))
      '()))

(define (df.eprogn e* env fenv denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (df.evaluate (car e*) env fenv denv)
                 (df.eprogn (cdr e*) env fenv denv))
          (df.evaluate (car e*) env fenv denv))
      empty-begin))

(define (df.make-function variables body env fenv)
  (lambda (values denv)
    (df.eprogn body (extend env variables values) fenv denv)))

(define env.global '())
(define fenv.global '())
(define denv.global '())

(define (definitial name . value)
  (if (null? value)
      (begin (set! env.global (cons (cons name 'void) env.global))
             name)
      (begin (set! env.global (cons (cons name (car value)) env.global))
             name)))

(define (definitial-function name . value)
  (if (null? value)
      (set! fenv.global (cons (cons name 'void) fenv.global))
      (set! fenv.global (cons (cons name (car value)) fenv.global))))

(define (defprimitive name value arity)
  (definitial-function name
    (lambda (values denv)
      (if (= arity (length values))
          (apply value values)
          (wrong "Incorrect arity" (list name values))))))

(definitial 't #t)
(definitial 'f #f)
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
(defprimitive '< < 2)

(definitial-function 'funcall
  (lambda (args)
    (if (> (length args) 1)
        (invoke (car args) (cdr args))
        (wrong "Incorrect arity" 'funcall))))

(define (lisp2-repl)
  (define (toplevel)
    (display (df.evaluate (read) env.global fenv.global denv.global))
    (toplevel))
  (toplevel))

(lisp2-repl)
