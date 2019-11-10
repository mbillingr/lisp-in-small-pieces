(import (libs book)
        (libs utils))

(include "../09-macros/evaluator.scm")

(include "10-02-code-walking.scm")
(include "10-03-boxes.scm")
(include "10-04-lambda-lifting.scm")
(include "10-05-extract.scm")
(include "10-06-gather-vars.scm")
(include "10-08-generating-c.scm")

; prepare root evaluator

(define root (create-evaluator #f))

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
  '(begin
    (+ (a! b-b bb) 2)
    'xyz
    (append '(1 2 3) '(4 2 3)))
  #t)
