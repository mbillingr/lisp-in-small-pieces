(define (make-primitive obj) obj)

(define primitive? procedure?)

(define (primitive-address obj) obj)

(define (defprimitive0 name value)
  (definitial name
    (let* ((arity+1 (+ 0 1))
           (behavior
            (lambda ()
              (if (= arity+1 (activation-frame-argument-length *val*))
                  (begin
                    (set! *val* (value arg1))
                    (set! *pc* (stack-pop)))
                  (signal-exception #t (list "Incorrect arity" name))))))
      (description-extend! name `(function ,name))
      (make-primitive behavior))))

(define (defprimitive1 name value)
  (definitial name
    (let* ((arity+1 (+ 1 1))
           (behavior
            (lambda ()
              (if (= arity+1 (activation-frame-argument-length *val*))
                  (let ((arg1 (activation-frame-argument *val* 0)))
                    (set! *val* (value arg1))
                    (set! *pc* (stack-pop)))
                  (signal-exception #t (list "Incorrect arity" name))))))
      (description-extend! name `(function ,name a))
      (make-primitive behavior))))

(define (defprimitive2 name value)
  (definitial name
    (let* ((arity+1 (+ 2 1))
           (behavior
            (lambda ()
              (if (= arity+1 (activation-frame-argument-length *val*))
                  (let ((arg1 (activation-frame-argument *val* 0))
                        (arg2 (activation-frame-argument *val* 1)))
                    (set! *val* (value arg1 arg2))
                    (set! *pc* (stack-pop)))
                  (signal-exception #t (list "Incorrect arity" name))))))
      (description-extend! name `(function ,name a b))
      (make-primitive behavior))))

(define (defprimitive3 name value)
  (definitial name
    (let* ((arity+1 (+ 3 1))
           (behavior
            (lambda ()
              (if (= arity+1 (activation-frame-argument-length *val*))
                  (let ((arg1 (activation-frame-argument *val* 0))
                        (arg2 (activation-frame-argument *val* 1))
                        (arg3 (activation-frame-argument *val* 2)))
                    (set! *val* (value arg1 arg2))
                    (set! *pc* (stack-pop)))
                  (signal-exception #t (list "Incorrect arity" name))))))
      (description-extend! name `(function ,name a b c))
      (make-primitive behavior))))

(define (defprimitive name value arity)
  (case arity
    ((0) (defprimitive0 name value))
    ((1) (defprimitive1 name value))
    ((2) (defprimitive2 name value))
    ((3) (defprimitive3 name value))
    (else static-wrong "Unsupported primitive arity" name arity)))

(definitial 't #t)
(definitial 'f #f)
(definitial 'nil '())
(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defprimitive 'pair? pair? 1)
(defprimitive 'symbol? symbol? 1)
(defprimitive 'eq? eq? 2)
(defprimitive 'null? null? 1)
(defprimitive 'set-car! set-car! 2)
(defprimitive 'set-cdr! set-cdr! 2)
(defprimitive '= = 2)
(defprimitive '< < 2)
(defprimitive '<= <= 2)
(defprimitive '> > 2)
(defprimitive '>= >= 2)
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defprimitive '* * 2)
(defprimitive '/ / 2)

(definitial 'call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (if (= arity+1 (activation-frame-argument-length *val*))
            (let ((f (activation-frame-argument *val* 0))
                  (frame (allocate-activation-frame (+ 1 1))))
              (set-activation-frame-argument!
                frame 0 (make-continuation (save-stack)))
              (set! *val* frame)
              (set! *fun* f)       ; useful for debug
              (invoke f #t))
            (signal-exception #t (list "Incorrect arity" 'call/cc)))))))

(definitial 'apply
  (let* ((arity 2)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (if (>= (activation-frame-argument-length *val*) arity+1)
            (let* ((proc (activation-frame-argument *val* 0))
                   (last-arg-index (- (activation-frame-argument-length *val*) 2))
                   (last-arg (activation-frame-argument *val* last-arg-index))
                   (size (+ last-arg-index (length last-arg)))
                   (frame (allocate-activation-frame size)))
              (define (copy-args i)
                (if (< i last-arg-index)
                    (begin (set-activation-frame-argument!
                             frame (- i 1) (activation-frame-argument *val* i))
                           (copy-args (+ i 1)))))
              (define (copy-args2 i last-arg)
                (if (not (null? last-arg))
                    (begin (set-activation-frame-argument!
                             frame i (car last-arg))
                           (copy-args2 (+ i 1) (cdr last-arg)))))
              (copy-args 1)
              (copy-args2 (- last-arg-index 1) last-arg)
              (set! *val* frame)
              (set! *fun* proc)       ; useful for debug
              (invoke proc #t))
            (signal-exception #t (list "Incorrect arity" 'apply)))))))
