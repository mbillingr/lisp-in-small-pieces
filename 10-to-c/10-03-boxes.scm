
(define-generic (insert-box! (o Program))
  (update-walk! insert-box! o))

(define-class Box-Read Program (reference))
(define-class Box-Write Program (reference form))
(define-class Box-Creation Program (variable))

(define-method (insert-box! (o Local-Reference))
  (if (Local-Variable-mutable? (Local-Reference-variable o))
      (make-Box-Read o)
      o))

(define-method (insert-box! (o Local-Assignment))
  (make-Box-Write (Local-Assignment-reference o)
                  (insert-box! (Local-Assignment-form o))))

(define-method (insert-box! (o Function))
  (set-Function-body!
    o (insert-box!
        (boxify-mutable-variables (Function-body o)
                                  (Function-variables o))))
  o)

(define-method (insert-box! (o Fix-Let))
  (set-Fix-Let-arguments! o (insert-box! (Fix-Let-arguments o)))
  (set-Fix-Let-body!
    o (insert-box!
        (boxify-mutable-variables (Fix-Let-body o)
                                  (Fix-Let-variables o))))
  o)

(define (boxify-mutable-variables form variables)
  (if (pair? variables)
      (if (Local-Variable-mutable? (car variables))
          (make-Sequence
            (make-Box-Creation (car variables))
            (boxify-mutable-variables form (cdr variables)))
          (boxify-mutable-variables form (cdr variables)))
      form))

(define-method (visualize (o Box-Creation) indent)
  (print-indented indent "boxify" (visualize (Box-Creation-variable o) 0)))

(define-method (visualize (o Box-Write) indent)
  (print-indented indent "box-set" (visualize (Reference-variable (Box-Write-reference o)) 0))
  (visualize (Box-Write-form o) (more indent)))

(define-method (visualize (o Box-Read) indent)
  (print-indented indent "box-get" (visualize (Reference-variable (Box-Read-reference o)) 0)))
