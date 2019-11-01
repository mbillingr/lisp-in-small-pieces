
(define-class Program Object ())

(define-class Reference Program (variable))
(define-class Local-Reference Reference ())
(define-class Global-Reference Reference ())
(define-class Predefined-Reference Reference ())

(define-class Global-Assignment Object (variable form))
(define-class Local-Assignment Object (reference form))

(define-class Function Program (variables body))
(define-class Alternative Program (condition consequent alternant))
(define-class Sequence Program (first last))
(define-class Constant Program (value))

(define-class Application Program ())
(define-class Regular-Application Application (function arguments))
(define-class Predefined-Application Application (variable arguments))
(define-class Fix-Let Program (variables arguments body))
(define-class Arguments Program (first others))
(define-class No-Argument Program ())

(define-class Variable Object (name))
(define-class Global-Variable Variable ())
(define-class Predefined-Variable Variable (description))
(define-class Local-Variable Variable (mutable? dotted?))

(define-class Magic-Keyword Object (name handler))

(define-class Environment Object (next))
(define-class Full-Environment Environment (variable))

(define-class Functional-Description Object (comparator arity text))



(include "visualize.scm")


(define (objectify e r)
  (if (atom? e)
      (cond ((Magic-Keyword? e) e)
            ((Program? e) e)
            ((symbol? e) (objectify-symbol e r))
            (else (objectify-quotation e r)))
      (let ((m (objectify (car e) r)))
        (if (Magic-Keyword? m)
            ((Magic-Keyword-handler m) e r)
            (objectify-application m (cdr e) r)))))


(define (objectify-quotation value r)
  (make-Constant value))

(define (objectify-alternative ec et ef r)
  (make-Alternative (objectify ec r)
                    (objectify et r)
                    (objectify ef r)))

(define (objectify-sequence e* r)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (let ((a (objectify (car e*) r)))
            (make-Sequence a (objectify-sequence (cdr e*) r)))
          (objectify (car e*) r))
      (make-Constant 42)))

(define (objectify-application ff e* r)
  (let ((ee* (convert2arguments (map (lambda (e) (objectify e r)) e*))))
    (cond ((Function? ff)
           (process-closed-application ff ee*))
          ((Predefined-Reference? ff)
           (let* ((fvf (Predefined-Reference-variable ff))
                  (desc (Predefined-Variable-description fvf)))
             (if (Functional-Description? desc)
                 (if ((Functional-Description-comparator desc)
                      (length e*) (Functional-Description-arity desc))
                     (make-Predefined-Application fvf ee*)
                     (objectify-error "Incorrect predefined arity" ff e*))
                 (make-Regular-Application ff ee*))))
          (else (make-Regular-Application ff ee*)))))

(define (process-closed-application f e*)
  (let ((v* (Function-variables f))
        (b (Function-body f)))
    (if (and (pair? v*) (Local-Variable-dotted? (car (last-pair v*))))
        (process-nary-closed-application f e*)
        (if (= (number-of e*) (length v*))
            (make-Fix-Let v* e* b)
            (objectify-error "Incorrect regular arity" f e*)))))

(define (convert2arguments e*)
  (if (pair? e*)
      (make-Arguments (car e*) (convert2arguments (cdr e*)))
      (make-No-Argument)))

(define-generic (number-of (o)) '*not-implemented*)
(define-method (number-of (o Arguments))
  (+ 1 (number-of (Arguments-others o))))
(define-method (number-of (o No-Argument)) 0)

(define (process-nary-closed-application f e*)
  (let* ((v* (Function-variables f))
         (b (Function-body f))
         (o (make-Fix-Let
              v*
              (let gather ((e* e*) (v* v*))
                (if (Local-Variable-dotted? (car v*))
                    (make-Arguments
                      (let pack ((e* e*))
                        (if (Arguments? e*)
                            (make-Predefined-Application
                              (find-variable? 'cons g.predef)
                              (make-Arguments
                                (Arguments-first e*)
                                (make-Arguments
                                  (pack (Arguments-others e*))
                                  (make-No-Argument))))
                            (make-Constant '())))
                      (make-No-Argument))
                    (if (Arguments? e*)
                        (make-Arguments (Arguments-first e*)
                                        (gather (Arguments-others e*)
                                                (cdr v*)))
                        (objectify-error "Incorrect dotted arity" f e*))))
              b)))
    (set-Local-Variable-dotted?! (car (last-pair v*)) #f)
    o))

(define (objectify-function names body r)
  (let* ((vars (objectify-variables-list names))
         (b    (objectify-sequence body (r-extend* r vars))))
    (make-Function vars b)))

(define (objectify-variables-list names)
  (if (pair? names)
      (cons (make-Local-Variable (car names) #f #f)
            (objectify-variables-list (cdr names)))
      (if (symbol? names)
          (list (make-Local-Variable names #f #t))
          '())))

(define (objectify-symbol variable r)
  (let ((v (find-variable? variable r)))
    (cond ((Magic-Keyword? v)       v)
          ((Local-Variable? v)      (make-Local-Reference v))
          ((Global-Variable? v)     (make-Global-Reference v))
          ((Predefined-Variable? v) (make-Predefined-Reference v))
          (else (objectify-free-global-reference variable r)))))

(define (objectify-free-global-reference name r)
  (let ((v (make-Global-Variable name)))
    (insert-global! v r)
    (make-Global-Reference v)))

(define (r-extend* r vars)
  (if (pair? vars)
      (r-extend (r-extend* r (cdr vars)) (car vars))
      r))

(define (r-extend r var)
  (make-Full-Environment r var))

(define (find-variable? name r)
  (if (Full-Environment? r)
      (let ((var (Full-Environment-variable r)))
        (if (eq? name
                 (cond ((Variable? var) (Variable-name var))
                       ((Magic-Keyword? var) (Magic-Keyword-name var))))
            var
            (find-variable? name (Full-Environment-next r))))
      (if (Environment? r)
          (find-variable? name (Environment-next r))
          #f)))

(define (insert-global! variable r)
  (let ((r (find-global-environment r)))
    (set-Environment-next!
      r (make-Full-Environment (Environment-next r) variable))))

(define (mark-global-preparation-environment g)
  (make-Environment g))

(define (find-global-environment r)
  (if (Full-Environment? r)
      (find-global-environment (Full-Environment-next r))
      r))

(define (objectify-assignment variable e r)
  (let ((ov (objectify variable r))
        (of (objectify e r)))
    (cond ((Local-Reference? ov)
           (set-Local-Variable-mutable?! (Local-Reference-variable ov) #t)
           (make-Local-Assignment ov of))
          ((Global-Reference? ov)
           (make-Global-Assignment (Global-Reference-variable ov) of))
          (else (objectify-error "Illegal mutated reference" variable)))))


(define special-if
  (make-Magic-Keyword 'if
    (lambda (e r) (objectify-alternative (cadr e) (caddr e) (cadddr e) r))))

(define special-begin
  (make-Magic-Keyword 'begin
    (lambda (e r) (objectify-sequence (cdr e) r))))

(define special-quote
  (make-Magic-Keyword 'quote
    (lambda (e r) (objectify-quotation (cadr e) r))))

(define special-set!
  (make-Magic-Keyword 'set!
    (lambda (e r) (objectify-assignment (cadr e) (caddr e) r))))

(define special-lambda
  (make-Magic-Keyword 'lambda
    (lambda (e r) (objectify-function (cadr e) (cddr e) r))))

;;; Backquote forms are supposed to be correct.  This is very ugly and
;;; only approximate. Backquoting should be better interleaved with
;;; objectification. What if unquote shadows lexically a comma ?
;;; QUICK and DIRTY!

(define special-quasiquote
  (make-Magic-Keyword
   'quasiquote
   (lambda (e r)
     (define (walk e)
       (if (pair? e)
           (if (eq? (car e) 'unquote)
               (cadr e)
               (if (eq? (car e) 'quasiquote)
                   (objectify-error "No embedded quasiquotation" e)
                   (walk-pair e)))
           (list special-quote e)))
     (define (walk-pair e)
       (if (pair? (car e))
           (if (eq? (car (car e)) 'unquote-splicing)
               (list (make-Predefined-Reference
                      (find-variable? 'append g.predef))
                     (cadr (car e))
                     (walk (cdr e)))
               (list (make-Predefined-Reference
                      (find-variable? 'cons g.predef))
                     (walk (car e))
                     (walk (cdr e))))
           (list (make-Predefined-Reference
                  (find-variable? 'cons g.predef))
                 (list special-quote (car e))
                 (walk (cdr e)))))
     (objectify (walk (cadr e)) r))))

(define *special-form-keywords*
  (list special-quote
        special-if
        special-begin
        special-set!
        special-lambda
        ; ...
        special-quasiquote))


(define-class Evaluator Object (mother
                                Preparation-Environment
                                RunTime-Environment
                                eval
                                expand))

(define (create-evaluator old-level)
  (let ((level 'wait)
        (g g.predef)
        (sg sg.predef))
    (define (expand e)
      (let ((prg (objectify e (Evaluator-Preparation-Environment level))))
        (enrich-with-new-global-variables! level)
        prg))
    (define (eval e)
      (let ((prg (expand e)))
        (evaluate prg (Evaluator-RunTime-Environment level))))
    ;; Create resulting evaluator instance
    (set! level (make-Evaluator old-level 'wait 'wait eval expand))
    ;; Enrich environment with eval
    (set! g (r-extend* g *special-form-keywords*))
    (set! g (r-extend* g (make-macro-environment level)))
    (let ((eval-var (make-Predefined-Variable
                      'eval (make-Functional-Description = 1 "")))
          (eval-fn (make-RunTime-Primitive eval = 1)))
      (set! g (r-extend g eval-var))
      (set! sg (sr-extend sg eval-var eval-fn)))
    ;; Mark the beginning of the global environment
    (set-Evaluator-Preparation-Environment!
      level (mark-global-preparation-environment g))
    (set-Evaluator-RunTime-Environment!
      level (mark-global-runtime-environment sg))
    level))

(define (make-macro-environment current-level)
  (let ((metalevel (delay (create-evaluator current-level))))
    (list (make-Magic-Keyword 'eval-in-abbreviation-world
            (special-eval-in-abbreviation-world metalevel))
          (make-Magic-Keyword 'define-abbreviation
            (special-define-abbreviation metalevel))
          (make-Magic-Keyword 'let-abbreviation
            (special-let-abbreviation metalevel))
          (make-Magic-Keyword 'with-aliases
            (special-with-aliases metalevel)))))

(define (special-eval-in-abbreviation-world level)
  (lambda (e r)
    (let ((body (cdr e)))
      (objectify ((Evaluator-eval (force level))
                  `(,special-begin . ,body))
                 r))))

(define (special-define-abbreviation level)
  (lambda (e r)
    (let* ((call      (cadr e))
           (body      (cddr e))
           (name      (car call))
           (variables (cdr call)))
      (let ((expander ((Evaluator-eval (force level))
                       `(,special-lambda ,variables . ,body))))
        (define (handler e r)
          (println "| expanding macro" e)
          (println "|              =>" (invoke expander (cdr e)))
          (objectify (invoke expander (cdr e)) r))
        (insert-global! (make-Magic-Keyword name handler) r)
        (objectify #t r)))))

(define (special-let-abbreviation level)
  (lambda (e r)
    (let ((level  (force level))
          (macros (cadr e))
          (body   (cddr e)))
      (define (make-macro def)
        (let* ((call      (cadr def))
               (body      (cddr def))
               (name      (car call))
               (variables (cdr call)))
          (let ((expander ((Evaluator-eval level)
                           `(,special-lambda ,variables . ,body))))
            (define (handler e r)
              (objectify (invoke expander (cdr e)) r))
            (make-Magic-Keyword name handler))))
      (objectify `(,special-begin . ,body)
                 (r-extend* r (map mace-macro macros))))))

(define (special-with-aliases level)
  (lambda (e current-r)
    (let* ((level   (force level))
           (oldr    (Evaluator-Preparation-Environment level))
           (oldsr   (Evaluator-Runtime-Environment level))
           (aliases (cadr e))
           (body    (cddr e)))
      (let bind ((aliases aliases)
                 (r       oldr)
                 (sr      oldsr))
        (if (pair? aliases)
            (let* ((variable (car (car aliases)))
                   (word     (cadr (car aliases)))
                   (var      (make-Local-Variable variable #f #f)))
              (bind (cdr aliases)
                    (r-extend r var)
                    (sr-extend sr var (objectify word current-r))))
            (let ((result 'wait))
              (set-Evaluator-Preparation-Environment! level r)
              (set-Evaluator-RunTime-Environment! level sr)
              (set! result (objectify `(,special-begin . ,body)
                                      current-r))
              (set-Evaluator-Preparation-Environment! level oldr)
              (set-Evaluator-RunTime-Environment! level oldsr)
              result))))))
