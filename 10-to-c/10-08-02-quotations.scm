
(define (generate-quotations out qv*)
  (cond ((pair? qv*)
         (format out "~%/* Quotations: */~%")
         (scan-quotations out qv* (length qv*) '()))))

(define (scan-quotations out qv* i results)
  (if (pair? qv*)
      (let* ((qv (car qv*))
             (value (Quotation-Variable-value qv))
             (other-qv (already-seen-value? value results)))
        (cond (other-qv
               (generate-quotation-alias out qv other-qv)
               (scan-quotations out (cdr qv*) i (cons qv results)))
              ((C-value? value)
               (generate-C-value out qv)
               (scan-quotations out (cdr qv*) i (cons qv results)))
              ((symbol? value)
               (scan-symbol out value qv* i results))
              ((pair? value)
               (scan-pair out value qv* i results))
              (else (generate-error "Unhandled constant" qv))))))

(define (already-seen-value? value qv*)
  (and (pair? qv*)
       (if (equal? value (Quotation-Variable-value (car qv*)))
           (car qv*)
           (already-seen-value? value (cdr qv*)))))

(define (generate-quotation-alias out qv1 qv2)
  (format out "#define thing~A thing~A /* ~S */~%"
          (Quotation-Variable-name qv1)
          (Quotation-Variable-name qv2)
          (Quotation-Variable-value qv2)))


(define *maximal-fixnum* 16384)
(define *minimal-fixnum* (- *maximal-fixnum*))

(define (C-value? value)
  (or (null? value)
      (boolean? value)
      (and (integer? value)
           (< *minimal-fixnum* value)
           (< value *maximal-fixnum*))
      (string? value)))

(define (generate-C-value out qv)
  (let ((value (Quotation-Variable-value qv))
        (index (Quotation-Variable-name qv)))
    (cond ((null? value)
           (format out "#define thing~A SCM_nil /* '() */~%"
                   index))
          ((boolean? value)
           (format out "#define thing~A ~A /* ~S */~%"
                   index (if value "SCM_true" "SCM_false") value))
          ((integer? value)
           (format out "#define thing~A SCM_Int2fixnum(~A)~%"
                   index value))
          ((string? value)
           (format out "SCM_DefineString(thing~A_object,\"~A\");~%"
                   index value)
           (format out "#define thing~A SCM_Wrap(&thing~A_object)~%"
                   index index)))))

(define (scan-symbol out value qv* i results)
  (let* ((qv (car qv*))
         (str (symbol->string value))
         (strqv (already-seen-value? str results)))
    (cond (strqv (generate-symbol out qv strqv)
                 (scan-quotations out (cdr qv*) i (cons qv results)))
          (else
            (let ((newqv (make-Quotation-Variable
                           i (symbol->string value))))
              (scan-quotations out (cons newqv qv*)
                               (+ i 1) results))))))

(define (generate-symbol out qv strqv)
  (format out "SCM_DefineSymbol(thing~A_object,thing~A);    /* ~S */~%"
          (Quotation-Variable-name qv)
          (Quotation-Variable-name strqv)
          (Quotation-Variable-value qv))
  (format out "#define thing~A SCM_Wrap(&thing~A_object)~%"
          (Quotation-Variable-name qv)
          (Quotation-Variable-name qv)))

(define (scan-pair out value qv* i results)
  (let* ((qv (car qv*))
         (d (cdr value))
         (dqv (already-seen-value? d results)))
    (if dqv
        (let* ((a (car value))
               (aqv (already-seen-value? a results)))
          (if aqv
              (begin (generate-pair out qv aqv dqv)
                     (scan-quotations out (cdr qv*) i (cons qv results)))
              (let ((newaqv (make-Quotation-Variable i a)))
                (scan-quotations out (cons newaqv qv*)
                                 (+ i 1) results))))
        (let ((newdqv (make-Quotation-Variable i d)))
          (scan-quotations out (cons newdqv qv*)
                           (+ i 1) results)))))

(define (generate-pair out qv aqv dqv)
  (format out
          "SCM_DefinePair(thing~A_object,thing~A,thing~A); /* ~S */~%"
          (Quotation-Variable-name qv)
          (Quotation-Variable-name aqv)
          (Quotation-Variable-name dqv)
          (Quotation-Variable-value qv))
  (format out
          "#define thing~A SCM_Wrap(&thing~A_object)~%"
          (Quotation-Variable-name qv) (Quotation-Variable-name qv)))
