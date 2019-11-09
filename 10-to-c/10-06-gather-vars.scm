
(define-class With-Temp-Function-Definition Function-Definition (temporaries))

(define-class Renamed-Local-Variable Variable (index))


(define (gather-temporaries! o)
  (set-Flattened-Program-definitions!
    o (map (lambda (def)
             (let ((flatfun (make-With-Temp-Function-Definition
                              (Function-Definition-variables def)
                              (Function-Definition-body def)
                              (Function-Definition-free def)
                              (Function-Definition-index def)
                              '())))
               (collect-temporaries! flatfun flatfun '())))
           (Flattened-Program-definitions o)))
  o)

(define-generic (collect-temporaries! (o Program) flatfun r)
  (update-walk! collect-temporaries! o flatfun r))

(define-method (collect-temporaries! (o Local-Reference) flatfun r)
  (let* ((variable (Local-Reference-variable o))
         (v (assq variable r)))
    (if (pair? v)
        (make-Local-Reference (cdr v))
        o)))

(define-method (collect-temporaries! (o Box-Creation) flatfun r)
  (let* ((variable (Box-Creation-variable o))
         (v (assq variable r)))
    (if (pair? v)
        (make-Box-Creation (cdr v))
        o)))

(define-method (collect-temporaries! (o Fix-Let) flatfun r)
  (set-Fix-Let-arguments!
    o (collect-temporaries! (Fix-Let-arguments o) flatfun r))
  (let* ((newvars (map new-renamed-variable
                       (Fix-Let-variables o)))
         (newr (append (map cons (Fix-Let-variables o) newvars) r)))
    (adjoin-temporary-variables! flatfun newvars)
    (set-Fix-Let-variables! o newvars)
    (set-Fix-Let-body!
      o (collect-temporaries! (Fix-Let-body o) flatfun newr))
    o))

(define (adjoin-temporary-variables! flatfun newvars)
  (let adjoin ((temps (With-Temp-Function-Definition-temporaries flatfun))
               (vars newvars))
    (if (pair? vars)
        (if (memq (car vars) temps)
            (adjoin temps (cdr vars))
            (adjoin (cons (car vars) temps) (cdr vars)))
        (set-With-Temp-Function-Definition-temporaries! flatfun temps))))


(define renaming-variables-counter 0)

(define-generic (new-renamed-variable (variable)))

(define-method (new-renamed-variable (variable Local-Variable))
  (set! renaming-variables-counter (+ renaming-variables-counter 1))
  (make-Renamed-Local-Variable
    (Variable-name variable) renaming-variables-counter))



(define-method (visualize (o Renamed-Local-Variable) indent)
  (string-append
    (symbol->string (Renamed-Local-Variable-name o))
    ":"
    (number->string (Renamed-Local-Variable-index o))))

(define-method (visualize (o With-Temp-Function-Definition) indent)
  (print-indented indent
    "define-tmp" (Function-Definition-index o) (map (lambda (v) (visualize v 0))
                                                    (Function-Definition-variables o)))
  (print-indented (more indent) "free-variables:" (map (lambda (v) (visualize v 0))
                                                       (Function-Definition-free o)))
  (print-indented (more indent) "temporaries:" (map (lambda (v) (visualize v 0))
                                                    (With-Temp-Function-Definition-temporaries o)))
  (visualize (Function-Definition-body o) (more indent)))
