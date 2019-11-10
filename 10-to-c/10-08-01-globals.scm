
(define (generate-global-environment out gv*)
  (if (pair? gv*)
      (begin (format out "~%/* Global environment: */~%")
             (for-each (lambda (gv) (generate-global-variable out gv))
                       gv*))))

(define (generate-global-variable out gv)
  (let ((name (Global-Variable-name gv)))
    (format out "SCM_DefineGlobalVariable(~A, \"~A\");~%"
            (IdScheme->IdC name) name)))

(define Scheme->C-names-mapping
  '((* . "TIMES")
    (< . "LESSP")
    (pair? . "CONSP")
    (set-cdr! . "RPLACD")))

(define (IdScheme->IdC name)
  (let ((v (assq name Scheme->C-names-mapping)))
    (if (pair? v)
        (cdr v)
        (let ((str (symbol->string name)))
          (let retry ((Cname (compute-Cname str)))
            (if (Cname-clash? Cname Scheme->C-names-mapping)
                (retry (compute-another-Cname str))
                (begin (set! Scheme->C-names-mapping
                             (cons (cons name Cname)
                                   Scheme->C-names-mapping))
                       Cname)))))))

(define (Cname-clash? Cname mapping)
  (let check ((mapping mapping))
    (and (pair? mapping)
         (or (string=? Cname (cdr (car mapping)))
             (check (cdr mapping))))))

(define compute-another-Cname
  (let ((counter 1))
    (lambda (str)
      (set! counter (+ 1 counter))
      (compute-Cname (format #f "~A_~A" str counter)))))

(define (compute-Cname str)
  (define (mapcan f l)
    (if (pair? l)
        (append (f (car l)) (mapcan f (cdr l)))
        '()))

  (define (convert-char char)
    (case char
      ((#\_)   '(#\_ #\_))
      ((#\?)   '(#\p))
      ((#\!)   '(#\i))
      ((#\<)   '(#\l))
      ((#\>)   '(#\g))
      ((#\=)   '(#\e))
      ((#\- #\/ #\* #\:)   '())
      (else (list char))))

  (let ((cname (mapcan convert-char (string->list str))))
    (if (pair? cname) (list->string cname) "weird")))
