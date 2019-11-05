(import (libs book)
        (libs utils))

(include "evaluator.scm")

(define root (create-evaluator #f))
(define eval (Evaluator-eval root))

(define ast ((Evaluator-expand root)
             '(lambda (x) x)))

(define (run-tests)
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

  ;((Evaluator-eval root)
  ; '(define-abbreviation (define var . body)
  ;    (if (pair? var)
  ;        `(set! ,(car var) ,(cons 'lambda (cons (cdr var) body)))
  ;        (cons `set! (cons var body)))

  ((Evaluator-eval root)
   '(define baba (+ 7 5)))
  (assert-eq 12 ((Evaluator-eval root) 'baba))

  ((Evaluator-eval root)
   '(define (baba alpha beta) (+ alpha beta)))
  (assert-eq 123 ((Evaluator-eval root) '(baba 100 23)))

  (assert-eq 720 ((Evaluator-eval root)
                  '((lambda (inner-factorial)
                       (set! inner-factorial (lambda (i acc)
                                               (if (< i 2) acc
                                                   (inner-factorial (- i 1) (* acc i)))))
                       (inner-factorial 6 1))
                    (quote *unassigned*))))

  ((Evaluator-eval root)
   '(define (factorial n)
      (define (inner-factorial i acc)
        (if (< i 2)
            acc
            (inner-factorial (- i 1) (* acc i))))
      (inner-factorial n 1)))
  (assert-eq 120 ((Evaluator-eval root) '(factorial 5)))
  ;(assert-eq 120 ((Evaluator-eval root) '(inner-factorial 5 1)))

  (assert-eq 42 ((Evaluator-eval root)
                 '(begin (define (outer)
                           (define my-answer 42)
                           (define (inner)
                             my-answer)
                           (inner))
                         (outer)))))
