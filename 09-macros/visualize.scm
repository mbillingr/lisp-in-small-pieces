(define INDENT 4)

(define-generic (visualize (o) indent)
  (error "not implemented:" 'visualize (object->class o)))

;(define-method (visualize (o Program) indent))
;  (print-indented indent "Program"))

(define-method (visualize (o Reference) indent)
  (print-indented indent "ref" (visualize (Reference-variable o) 0)))

(define-method (visualize (o Local-Reference) indent)
  (print-indented indent "local-ref" (visualize (Local-Reference-variable o) 0)))

(define-method (visualize (o Global-Reference) indent)
  (print-indented indent "global-ref" (visualize (Global-Reference-variable o) 0)))

(define-method (visualize (o Predefined-Reference) indent)
  (print-indented indent "predef-ref" (visualize (Predefined-Reference-variable o) 0)))

(define-method (visualize (o Global-Assignment) indent)
  (print-indented indent "global-set")
  (print-indented (more indent) (visualize (Global-Assignment-variable o) 0))
  (visualize (Global-Assignment-form o) (more indent)))

(define-method (visualize (o Local-Assignment) indent)
  (print-indented indent "local-set" (visualize (Reference-variable (Local-Assignment-reference o)) 0))
  (visualize (Local-Assignment-form o) (more indent)))

(define-method (visualize (o Function) indent)
  (print-indented indent
    "lambda" (map (lambda (v) (visualize v 0))
                  (Function-variables o)))
  (visualize (Function-body o) (more indent)))

(define-method (visualize (o Alternative) indent)
  (print-indented indent "if" (Alternative-condition o))
  (visualize (Alternative-consequent o) (+ 3 indent))
  (visualize (Alternative-alternant o) (+ 3 indent)))

(define-method (visualize (o Sequence) indent)
  (print-indented indent "sequence")
  (visualize (Sequence-first o) (more indent))
  (visualize (Sequence-last o) (more indent)))

(define-method (visualize (o Constant) indent)
  (print-indented indent "const" (Constant-value o)))

(define-method (visualize (o Application) indent)
  (print-indented indent "*naked application*"))

(define-method (visualize (o Regular-Application) indent)
  (print-indented indent "regular-application")
  (visualize (Regular-Application-function o) (more indent))
  (print-indented (more indent) "arguments")
  (visualize (Regular-Application-arguments o) (more (more indent))))

(define-method (visualize (o Predefined-Application) indent)
  (print-indented indent "predef-application")
  (print-indented (more indent) (visualize (Predefined-Application-variable o) 0))
  (print-indented (more indent) "arguments")
  (visualize (Predefined-Application-arguments o) (more (more indent))))

(define-method (visualize (o Fix-Let) indent)
  (print-indented indent "fix-let" (Fix-Let-variables))
  (print-indented (more indent) "arguments")
  (visualize (Fix-Let-arguments o) (more (more indent)))
  (visualize (Fix-Let-body o) (more indent)))

(define-method (visualize (o Arguments) indent)
  (visualize (Arguments-first o) indent)
  (visualize (Arguments-others o) indent))

(define-method (visualize (o No-Argument) indent) "")


(define-method (visualize (o Variable) indent)
  (string-append "var " (symbol->string (Variable-name o))))

(define-method (visualize (o Variable) indent)
  (symbol->string (Variable-name o)))

(define-method (visualize (o Magic-Keyword) indent)
  (string-append "keyword " (symbol->string (Magic-Keyword-name o))))


(define (print-indented indent . args)
  (if (> indent 0)
      (begin (display " ")
             (apply print-indented (- indent 1) args))
      (apply println args)))

(define (more indent)
  (+ indent INDENT))
