
(define-class Functional-Description Object (comparator arity generator))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name Cname arity)
     (let ((v (make-Predefined-Variable
                'name (make-Functional-Description
                        = arity
                        (make-predefined-application-generator 'Cname)))))
       (set! g.init (cons v g.init))
       'name))))

(define (make-predefined-application-generator Cname)
  (lambda (e out)
    (format out "~A" Cname)
    (between-parentheses out
      (arguments->C (Predefined-Application-arguments e) out))))

(define g.init '())

(defprimitive cons "SCM_cons" 2)
(defprimitive car "SCM_car" 1)
(defprimitive + "SCM_Plus" 2)
(defprimitive = "SCM_EqnP" 2)
