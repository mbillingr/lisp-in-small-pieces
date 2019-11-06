
(define-class Flattened-Program Program (form quotations definitions))
(define-class Quotation-Variable Variable (value))
(define-class Function-Definition Flat-Function (index))
(define-class Closure-Creation Program (index variables free))

(define (extract-things! o)
  (let ((result (make-Flattened-Program o '() '())))
    (set-Flattened-Program-form! result (extract! o result))
    result))

(define-generic (extract! (o Program) result)
  (update-walk! extract! o result))

(define-method (extract! (o Constant) result)
  (let* ((qv* (Flattened-Program-quotations result))
         (qv (make-Quotation-Variable (length qv*)
                                      (Constant-value o))))
    (set-Flattened-Program-quotations! result (cons qv qv*))
    (make-Global-Reference qv)))

(define-method (extract! (o Flat-Function) result)
  (let* ((newbody (extract! (Flat-Function-body o) result))
         (variables (Flat-Function-variables o))
         (freevars (let extract ((free (Flat-Function-free o)))
                     (if (Free-Environment? free)
                         (cons (Reference-variable (Free-Environment-first free))
                               (extract (Free-Environment-others free)))
                         '())))
         (index (adjoin-definition!
                  result variables newbody freevars)))
    (make-Closure-Creation index variables (Flat-Function-free o))))

(define (adjoin-definition! result variables body free)
  (let* ((definitions (Flattened-Program-definitions result))
         (newindex (length definitions)))
    (set-Flattened-Program-definitions!
      result (cons (make-Function-Definition variables body free newindex)
                   definitions))
    newindex))

(define (closurize-main! o)
  (let ((index (length (Flattened-Program-definitions o))))
    (set-Flattened-Program-definitions!
      o (cons (make-Function-definition
                '() (Flattened-Program-form o) '() index)
              (Flattened-Program-definitions o)))
    (set-Flattened-Program-form!
      o (make-Regular-Application
          (make-Closure-Creation index '() (make-No-Free))
          (make-No-Argument)))
    o))



(define-method (visualize (o Flattened-Program) indent)
  (print-indented indent "flat-program")
  (print-indented (more indent) "quotations")
  (for-each (lambda (qv) (print-indented (more (more indent))
                                         (Quotation-Variable-name qv) ":" (Quotation-Variable-value qv)))
            (Flattened-Program-quotations o))
  (print-indented (more indent) "definitions")
  (for-each (lambda (fn) (visualize fn (more (more indent))))
            (Flattened-Program-definitions o))
  (visualize (Flattened-Program-form o) (more indent)))

(define-method (visualize (o Quotation-Variable) indent)
  (number->string (Quotation-Variable-name o)))

(define-method (visualize (o Function-Definition) indent)
  (print-indented indent
    "define" (Function-Definition-index o) (map (lambda (v) (visualize v 0))
                                                (Function-Definition-variables o)))
  (print-indented (more indent) "free-variables:" (map (lambda (v) (visualize v 0))
                                                       (Function-Definition-free o)))
  (visualize (Function-Definition-body o) (more indent)))

(define-method (visualize (o Closure-Creation) indent)
  (print-indented indent "closure" (Closure-Creation-index o))
  (print-indented (more indent) (Closure-Creation-variables o))
  (print-indented (more indent) (Closure-Creation-free o)))
