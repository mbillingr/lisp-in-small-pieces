
(define-generic (->C (e Program) out))

(define-syntax between-parentheses
  (syntax-rules ()
    ((between-parentheses out . body)
     (let ((o out))
       (format o "(")
       (begin . body)
       (format o ")")))))

;; Compiling References to Variables

(define-method (->C (e Reference) out)
  (reference->C (Reference-variable e) out))

(define-generic (reference->C (v Variable) out))

(define-method (reference->C (v Variable) out)
  (variable->C v out))

(define-generic (variable->C (variable) out))

(define-method (variable->C (variable Variable) out)
  (format out (IdScheme->IdC (Variable-name variable))))

(define-method (variable->C (variable Renamed-Local-Variable) out)
  (format out "~A_~A"
              (IdScheme->IdC (Variable-name variable))
              (Renamed-Local-Variable-index variable)))

(define-method (variable->C (variable Quotation-Variable) out)
  (format out "thing~A" (Quotation-Variable-name variable)))

(define-method (reference->C (v Global-Variable) out)
  (format out "SCM_CheckedGlobal")
  (between-parentheses out
    (variable->C v out)))

(define-method (->C (e Free-Reference) out)
  (format out "SCM_Free")
  (between-parentheses out
    (variable->C (Free-Reference-variable e) out)))

;; Compiling Assignments

(define-method (->C (e Global-Assignment) out)
  (between-parentheses out
    (variable->C (Global-Assignment-variable e) out)
    (format out "=")
    (->C (Global-Assignment-form e) out)))

;; Compiling Boxes

(define-method (->C (e Box-Read) out)
  (format out "SCM_Content")
  (between-parentheses out
    (->C (Box-Read-reference e) out)))

(define-method (->C (e Box-Write) out)
  (between-parentheses out
    (format out "SCM_Content")
    (between-parentheses out
      (->C (Box-Write-reference e) out))
    (format out "=")
    (->C (Box-Write-form e) out)))

(define-method (->C (e Box-Creation) out)
  (variable->C (Box-Creation-variable e) out)
  (format out "= SCM_allocate_box")
  (between-parentheses out
    (variable->C (Box-Creation-variable e) out)))

;; Compiling Alternatives

(define-method (->C (e Alternative) out)
  (between-parentheses out
    (boolean->C (Alternative-condition e) out)
    (format out "~%? ")
    (->C (Alternative-consequent e) out)
    (format out "~%: ")
    (->C (Alternative-alternant e) out)))

(define-generic (boolean->C (e) out)
  (between-parentheses out
    (->C e out)
    (format out " != SCM_false")))

;; Compiling Sequences

(define-method (->C (e Sequence) out)
  (between-parentheses out
    (->C (Sequence-first e) out)
    (format out ",~%")
    (->C (Sequence-last e) out)))
