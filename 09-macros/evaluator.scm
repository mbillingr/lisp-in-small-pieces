(import (libs book)
        (libs utils))

(include "system.scm")

(define-class undefined-value Object ())

(define-class RunTime-Primitive Object (func comparator arity))
(define-class RunTime-Procedure Object (body variables environment))

(define objectify-error error)

(define g.predef '())
(define sg.predef '())


(define (sr-extend sr variable value)
  (cons (cons variable value) sr))

(define (sr-extend* sr variables values)
  (if (pair? variables)
      (if (Local-Variable-dotted? (car variables))
          (sr-extend sr (car variables) values)
          (if (pair? values)
              (sr-extend (sr-extend* sr (cdr variables) (cdr values))
                         (car variables) (car values))
              (evaluate-error "Not enough values" variables)))
      (if (null? values)
          sr
          (evaluate-error "Too many values" values))))

(define global-runtime-environment-mark (list (gensym)))

(define (mark-global-runtime-environment sg)
  (cons global-runtime-environment-mark sg))

(define (find-global-runtime-environment sg)
  (if (eq? (car sg) global-runtime-environment-mark)
      sg
      (find-global-runtime-environment (cdr sg))))

(define (enrich-with-new-global-variables! level)
  (let* ((g (find-global-environment
             (Evaluator-Preparation-Environment level)))
         (sg-head (find-global-runtime-environment
                   (Evaluator-RunTime-Environment level))))
    (let loop ((g (Environment-next g)))
      (if (Full-Environment? g)
          (let ((var (Full-Environment-variable g)))
            (if (and (Global-Variable? var)
                     (not (pair? (assq var sg-head))))
                (begin
                  (set-cdr! sg-head
                            (sr-extend (cdr sg-head) var undefined-value))))
            (loop (Full-Environment-next g)))))))


(define-generic (evaluate (o) sr)
  (error "not implemented:" 'evaluate (object->class o)))

(define-method (evaluate (o Constant) sr)
  (Constant-value o))

(define-method (evaluate (o Reference) sr)
  (let ((slot (assq (Reference-variable o) sr)))
    (if slot
        (cdr slot)
        (error "Undefined variable" (Variable-name (Reference-variable o))))))

(define-method (evaluate (o Local-Assignment) sr)
  (let ((slot (assq (Reference-variable (Local-Assignment-reference o)) sr)))
    (if slot
        (set-cdr! slot (evaluate (Local-Assignment-form o) sr))
        (error "Undefined variable"
               (Variable-name
                 (Reference-variable
                   (Local-Assignment-reference o)))))))

(define-method (evaluate (o Global-Assignment) sr)
  (let ((slot (assq (Global-Assignment-variable o) sr)))
    (if slot
        (set-cdr! slot (evaluate (Global-Assignment-form o) sr))
        (error "Undefined variable"
               (Variable-name (Global-Assignment-variable o))))))

(define-method (evaluate (o Function) sr)
  (make-RunTime-Procedure (Function-body o)
                          (Function-variables o)
                          sr))

(define-method (evaluate (o Alternative) sr)
  (if (evaluate (Alternative-condition o) sr)
      (evaluate (Alternative-consequent o) sr)
      (evaluate (Alternative-alternant o) sr)))

(define-method (evaluate (o Sequence) sr)
  (evaluate (Sequence-first o) sr)
  (evaluate (Sequence-last o) sr))

(define-method (evaluate (o Regular-Application) sr)
  (let ((func (evaluate (Regular-Application-function o) sr))
        (args (evaluate (Regular-Application-arguments o) sr)))
    (invoke func args)))

(define-method (evaluate (o Predefined-Application) sr)
  (let ((func (cdr (assq (Predefined-Application-variable o) sr)))
        (args (evaluate (Predefined-Application-arguments o) sr)))
    (invoke func args)))

(define-method (evaluate (o Fix-Let) sr)
  (evaluate (Fix-Let-body o)
            (sr-extend* sr
                        (Fix-Let-variables o)
                        (evaluate (Fix-Let-arguments o) sr))))

(define-method (evaluate (o Arguments) sr)
  (cons (evaluate (Arguments-first o) sr)
        (evaluate (Arguments-others o) sr)))

(define-method (evaluate (o No-Argument) sr)
  '())


(define-generic (invoke (func) args)
  (error "not implemented" 'invoke (object->class func)))

(define-method (invoke (f RunTime-Procedure) args)
  (if (let check ((variables (RunTime-Procedure-variables f))
                  (args args))
        (if (pair? variables)
            (or (Local-Variable-dotted? (car variables))
                ;; (car variables) is a regular variable
                (and (pair? args)
                     (check (cdr variables) (cdr args))))
            (not (pair? args))))
      (evaluate (RunTime-Procedure-body f)
                (sr-extend* (RunTime-Procedure-environment f)
                            (RunTime-Procedure-variables f)
                            args))
      (evaluate-error "Wrong arity" f args)))

(define-method (invoke (f RunTime-Primitive) args)
  (if ((RunTime-Primitive-comparator f) (length args)
                                        (RunTime-Primitive-arity f))
      (apply (RunTime-Primitive-func f) args)
      (evaluate-error "Wrong arity" f args)))

(define-syntax definitial
  (syntax-rules ()
    ((definitial name value)
     (let ((v (make-Predefined-Variable 'name #f)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v value))
       'name))))

(define-syntax defprimitive
  (syntax-rules (>=0 >=2)
    ((defprimitive name value 0)
     (let ((v (make-Predefined-Variable
               'name (make-Functional-Description = 0 "")))
           (f (make-RunTime-Primitive value = 0)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value 1)
     (let ((v (make-Predefined-Variable
               'name (make-Functional-Description = 1 "")))
           (f (make-RunTime-Primitive value = 1)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value 2)
     (let ((v (make-Predefined-Variable
               'name (make-Functional-Description = 2 "")))
           (f (make-RunTime-Primitive value = 2)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value 3)
     (let ((v (make-Predefined-Variable
               'name (make-Functional-Description = 3 "")))
           (f (make-RunTime-Primitive value = 3)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value >=0)
     (let ((v (make-Predefined-Variable
               'name (make-Functional-Description >= 0 "")))
           (f (make-RunTime-Primitive value >= 0)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))
    ((defprimitive name value >=2)
     (let ((v (make-Predefined-Variable
               'name (make-Functional-Description >= 2 "")))
           (f (make-RunTime-Primitive value >= 2)))
       (set! g.predef  (make-Full-Environment g.predef v))
       (set! sg.predef (sr-extend sg.predef v f))
       'name))))

(definitial t #t)
(definitial f #f)
(definitial nil '())

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive pair? pair? 1)
(defprimitive atom? atom? 1)
(defprimitive symbol? symbol? 1)
(defprimitive null? null? 1)
(defprimitive not not 1)
(defprimitive eq? eq? 2)
(defprimitive set-car! set-car! 2)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive = = 2)
(defprimitive < < 2)
(defprimitive > > 2)
(defprimitive * * 2)
(defprimitive <= <= 2)
(defprimitive >= >= 2)
(defprimitive remainder remainder 2)
(defprimitive modulo modulo 2)
(defprimitive display display 1)
(defprimitive newline newline 0)
(defprimitive list list >=0)
(defprimitive append append >=2)

(define root (create-evaluator #f))
(define eval (Evaluator-eval root))

(define ast ((Evaluator-expand root)
             '(lambda (x) x)))

(assert-eq 42 ((Evaluator-eval root) 42))

(assert-eq undefined-value ((Evaluator-eval root) 'x))
((Evaluator-eval root) '(set! x 123))
(assert-eq 123 ((Evaluator-eval root) 'x))
(assert-eq 2 ((Evaluator-eval root) '(if #t 2 3)))
(assert-eq 3 ((Evaluator-eval root) '(if #f 2 3)))
(assert-eq 456 ((Evaluator-eval root) '(begin (set! x 666) 456)))
(assert-eq 666 ((Evaluator-eval root) 'x))
(assert-instance RunTime-Procedure
                 ((Evaluator-eval root) '(lambda (x) x)))
(assert-instance RunTime-Procedure
                 ((Evaluator-eval root) '(lambda x x)))
((Evaluator-eval root) '(set! foo (lambda (x) x)))
(assert-eq 7 ((Evaluator-eval root) '(foo 7)))
((Evaluator-eval root) '(set! foo (lambda x x)))
(assert-equal '(7 11 13) ((Evaluator-eval root) '(foo 7 11 13)))
(assert-eq '() ((Evaluator-eval root) 'nil))
(assert-instance RunTime-Primitive ((Evaluator-eval root) '+))
(assert-eq 3 ((Evaluator-eval root) '(+ 1 2)))
(assert-eq 25 ((Evaluator-eval root) '((lambda (x) (* x x)) 5)))
(assert-eq 987 ((Evaluator-eval root)
                '(begin (set! fib
                              (lambda (n)
                                (if (< n 2)
                                    1
                                    (+ (fib (- n 1)) (fib (- n 2))))))
                        (fib 15))))

((Evaluator-eval root) '(set! y 12))
((Evaluator-eval root)
 '(define-abbreviation (bar x) `(cons ,x 3)))
((Evaluator-eval root) '(set! x 123))
((Evaluator-eval root) '(set! y 34))
(assert-equal (cons 36 3) ((Evaluator-eval root) '(bar (+ y 2))))

((Evaluator-eval root)
 '(define-abbreviation (delayed x) `(lambda () ,x)))
((Evaluator-eval root)
 '(define-abbreviation (force x) `(,x)))
((Evaluator-eval root) '(set! dd (delayed (* 12 3))))
(assert-instance RunTime-Procedure ((Evaluator-eval root) 'dd))
(assert-eq 36 ((Evaluator-eval root) '(force dd)))
(assert-eq 42 ((Evaluator-eval root) '((lambda (x) (set! x 42) x) 5)))

((Evaluator-eval root)
 '(define-abbreviation (let defs . body)
    ((lambda (names)
       (set! names
             (lambda (d)
               (if (pair? d)
                   (cons (car (car d))
                         (names (cdr d)))
                   d)))
       (set! values
             (lambda (d)
               (if (pair? d)
                   (cons (car (cdr (car d)))
                         (values (cdr d)))
                   d)))
       (cons (append `(lambda ,(names defs))
                     body)
             (values defs)))
     'names-uninit)))

(assert-eq 32
           ((Evaluator-eval root)
            '(let ((x 42)
                   (y 10))
               (- x y))))
