
(include "10-08-01-globals.scm")
(include "10-08-02-quotations.scm")
(include "10-08-04-expressions.scm")
(include "10-08-05-apply.scm")
(include "10-08-06-predefined.scm")
(include "10-08-07-functions.scm")
(include "10-08-08-main.scm")

(define (compile->C e out)
  ;(set! g.current '())
  (let ((prg (extract-things! (lift! (insert-box! (Sexp->object e))))))
    (gather-temporaries! (closurize-main! prg))
    (generate-C-program out e prg)))

(define (generate-C-program out e prg)
  (generate-header out e)
  ;(generate-global-environment out (g.current))
  (generate-global-environment out (get-globals root))
  (generate-quotations out (Flattened-Program-quotations prg))
  (generate-functions out (Flattened-Program-definitions prg))
  (generate-main out (Flattened-Program-form prg))
  (generate-trailer out)
  prg)

(define (generate-header out e)
  (format out "/* Compiler to C $Revision: 4.1 $ ~%")
  ;(pp e out))
  (format out "*/~%~%#include \"scheme.h\"~%"))

(define (generate-trailer out)
  (format out "~%/* End of generated code. */~%"))

(define (Sexp->object exp)
  ((Evaluator-expand root) exp))


(define (get-globals evaluator)
  (let* ((g (find-global-environment
             (Evaluator-Preparation-Environment evaluator))))
    (let loop ((g (Environment-next g))
               (vars '()))
      (if (Full-Environment? g)
          (let ((var (Full-Environment-variable g)))
            (if (Global-Variable? var)
                (loop (Full-Environment-next g) (cons var vars))
                (loop (Full-Environment-next g) vars)))
          vars))))
