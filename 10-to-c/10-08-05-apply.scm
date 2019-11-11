
(define-method (->C (e Regular-Application) out)
  (let ((n (number-of (Regular-Application-arguments e))))
    (cond ((< n 4)
           (format out "SCM_invoke~A" n)
           (between-parentheses out
             (->C (Regular-Application-function e) out)
             (->C (Regular-Application-arguments e) out)))
          (else (format out "SCM_invoke")
                (between-parentheses out
                  (->C (Regular-Application-function e) out)
                  (format out ",~A" n)
                  (->C (Regular-Application-arguments e) out))))))

(define-method (->C (e Arguments) out)
  (format out ",~%")
  (->C (Arguments-first e) out)
  (->C (Arguments-others e) out))

(define-method (->C (e No-Argument) out)
  #t)

(define-method (->C (e Fix-Let) out)
  (between-parentheses out
    (bindings->C (Fix-Let-arguments e) (Fix-Let-variables e) out)
    (->C (Fix-Let-body e) out)))

; careful: parameter order changed from the book!
(define-generic (bindings->C (arguments) variables out))

(define-method (bindings->C (e Arguments) variables out)
  (variable->C (car variables) out)
  (format out "=")
  (->C (Arguments-first e) out)
  (format out ",~%")
  (bindings->C (Arguments-others e) (cdr variables) out))

(define-method (bindings->C (e No-Argument) variables out)
  (format out ""))

(define-method (->C (e Predefined-Application) out)
  ((Functional-Description-generator
     (Predefined-Variable-description
       (Predefined-Application-variable e))) e out))

(define-generic (arguments->C (e) out))

(define-method (arguments->C (e Arguments) out)
  (->C (Arguments-first e) out)
  (->C (Arguments-others e) out))

(define-method (arguments->C (e No-Argument) out)
  #t)
