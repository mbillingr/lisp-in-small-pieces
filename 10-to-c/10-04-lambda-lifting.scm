
(define-class Flat-Function Function (free))
(define-class Free-Environment Program (first others))
(define-class No-Free Program ())
(define-class Free-Reference Reference ())

(define (lift! o)
  (lift-procedures! o #f '()))

(define-generic (lift-procedures! (o Program) flatfun vars)
  (update-walk! lift-procedures! o flatfun vars))


(define-method (lift-procedures! (o Local-Reference) flatfun vars)
  (let ((v (Local-Reference-variable o)))
    (if (memq v vars)
        o (begin (adjoin-free-variables! flatfun o)
                 (make-Free-Reference v)))))

(define (adjoin-free-variables! flatfun ref)
  (if (Flat-Function? flatfun)
      (let check ((free* (Flat-Function-free flatfun)))
        (if (No-Free? free*)
            (set-Flat-Function-free!
              flatfun (make-Free-Environment
                        ref (Flat-Function-free flatfun)))
            (if (eq? (Reference-variable ref)
                     (Reference-variable (Free-Environment-first free*)))
                'done
                (check (Free-Environment-others free*)))))))

(define-method (lift-procedures! (o Fix-Let) flatfun vars)
  (set-Fix-Let-arguments!
    o (lift-procedures! (Fix-Let-arguments o) flatfun vars))
  (let ((newvars (append (Fix-Let-variables o) vars)))
    (set-Fix-Let-body!
      o (lift-procedures! (Fix-Let-body o) flatfun newvars))
    o))

(define-method (lift-procedures! (o Function) flatfun vars)
  (let* ((localvars (Function-variables o))
         (body (Function-body o))
         (newfun (make-Flat-Function localvars body (make-No-Free))))
    (set-Flat-Function-body!
      newfun (lift-procedures! body newfun localvars))
    (let ((free* (Flat-Function-free newfun)))
      (set-Flat-Function-free!
        newfun (lift-proceduces! free* flatfun vars))
      newfun)))
