(define-library (sunny record)
    (export define-record-type)
    (import (sunny core)
            (sunny conditionals)
            (sunny exception)
            (sunny lists)
            (sunny ports))
    (begin
      (define <undefined> (if #f #f))

      (define-syntax define-record-type
        (syntax-rules ()
          ((define-record-type "field-names")
           '())
          ((define-record-type "field-names" field1 field2 ...)
           (cons (car 'field1)
                 (define-record-type "field-names" field2 ...)))

          ((define-record-type "rest-items" () ())
           '())
          ((define-record-type "rest-items" () (field1 field2 ...))
           (cons #f (define-record-type "rest-items" () (field2 ...))))
          ((define-record-type "rest-items" (init1 init2 ...) (field1 field2 ...))
           (define-record-type "rest-items" (init2 ...) (field2 ...)))

          ((define-record-type "field" idx (name accessor))
           (define (accessor obj) (vector-ref obj idx)))

          ((define-record-type "field" idx (name accessor modifier))
           (begin
             (define (accessor obj) (vector-ref obj idx))
             (define (modifier obj val) (vector-set! obj idx val))))

          ((define-record-type "fields" idx*)
           <undefined>)
          ((define-record-type "fields" (idx ...) field1 field2 ...)
           (begin
             (define-record-type "field" (+ idx ...) field1)
             (define-record-type "fields" (1 idx ...) field2 ...)))

          ((define-record-type name (constructor init ...) pred field ...)
           (begin
             (define name (vector 'name))
             (define (constructor init ...)
               (apply vector name init ... (define-record-type "rest-items" (init ...) (field ...))))
             (define (pred obj) (and (vector? obj)
                                     (eq? (vector-ref obj 0)
                                          name)))
             (define-record-type "fields" (1) field ...)))))))
