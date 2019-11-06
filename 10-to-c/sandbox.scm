(import (libs book)
        (libs utils))

(include "../09-macros/evaluator.scm")

(include "10-02-code-walking.scm")
(include "10-03-boxes.scm")
(include "10-04-lambda-lifting.scm")


(define (Sexp->object exp)
  (define root (create-evaluator #f))
  ((Evaluator-expand root) exp))


(visualize (insert-box! (Sexp->object '(lambda (x y) (set! y x) y))) 0)

(visualize
  (lift!
    (insert-box!
      (Sexp->object
        '(lambda (n)
            (lambda ()
              (set! n (+ n 1))
              n)))))
  0)
