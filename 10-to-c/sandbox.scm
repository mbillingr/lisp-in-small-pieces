(import (libs book)
        (libs utils))

(include "../09-macros/evaluator.scm")

(include "10-02-code-walking.scm")
(include "10-03-boxes.scm")
(include "10-04-lambda-lifting.scm")
(include "10-05-extract.scm")
(include "10-06-gather-vars.scm")
(include "10-08-generating-c.scm")

(define (create-compiler old-level)
  (let ((level 'wait)
        (g g.init))
        ;(sg sg.predef))
    (define (expand e)
      (let ((prg (objectify e (Evaluator-Preparation-Environment level))))
        ;(enrich-with-new-global-variables! level)
        prg))
    (define (eval e)
      (error "This compiler does not evaluate"))
    ;; Create resulting evaluator instance
    (set! level (make-Evaluator old-level 'wait 'wait eval expand))
    ;; Enrich environment with eval
    (set! g (r-extend* g *special-form-keywords*))
    (set! g (r-extend* g (make-macro-environment level)))
    ;(let ((eval-var (make-Predefined-Variable
    ;                  'eval (make-Functional-Description = 1 ""))
    ;      (eval-fn (make-RunTime-Primitive eval = 1))
    ;  (set! g (r-extend g eval-var)))
      ;(set! sg (sr-extend sg eval-var eval-fn)))
    ;; Mark the beginning of the global environment
    (set-Evaluator-Preparation-Environment!
      level (mark-global-preparation-environment g))
    ;(set-Evaluator-RunTime-Environment!
    ;  level (mark-global-runtime-environment sg))
    level))

; prepare root compiler/evaluator

(define root (create-compiler #f))

((Evaluator-expand root)
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

;;;

(visualize (insert-box! (Sexp->object '(lambda (x y) (set! y x) y))) 0)

(visualize
  (extract-things!
    (lift!
      (insert-box!
        (Sexp->object
          '((lambda (n)
               (lambda ()
                 (set! n (+ n 1))
                 n))
            10)))))
  0)

(visualize
  (gather-temporaries!
    (closurize-main!
      (extract-things!
        (lift!
          (insert-box!
            (Sexp->object
              '((lambda ()
                  (let ((x 10)))

                  (let ((x 20))
                    0)))))))))
  0)

(compile->C
  ;'(begin (set! foo (lambda x x))
  ;        (foo 1 2 3)

  '(let ((x 10)
         (y 20))
    (- x y))

  ;'(cons 1 2)
  #t)
