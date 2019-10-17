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

(defprimitive 'display display 1)
(defprimitive 'newline newline 0)

(definitial 'list
  (let* ((arity 0)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering list")
        (set! *val* (activation-frame-argument *val* 0))
        (set! *pc* (stack-pop))))))

(definitial 'read
  (let* ((arity 0)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering read")
        (if (= arity+1 (activation-frame-argument-length *val*))
            (begin (set! *val* (read))
                   (set! *pc* (stack-pop)))
            (signal-exception #t (list "Incorrect arity" 'read)))))))


(definitial 'eof-object?
  (let* ((arity 0)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering eof-object?")
        (set! *val* #f)
        (set! *pc* (stack-pop))))))

;(definitial 'the-environment
;  (let* ((arity 0)
;         (arity+1 (+ arity 1)))))
;    (make-primitive
;      (lambda ()
;        (if (= arity+1 (activation-frame-argument-length *val*))
;            (begin (set! *val* (export))
;                   (set! *pc* stack-pop)
;            (signal-exception #t (list "Incorrect arity" 'the-environment)))))))

(definitial 'call/cc
  (let* ((arity 1)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering call/cc")
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
        (println "entering apply")
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
              (println *fun*)
              (println *val*)
              (invoke proc #t))
            (signal-exception #t (list "Incorrect arity" 'apply)))))))

(definitial 'enrich
  (let* ((arity 1)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering enrich")
        (if (>= (activation-frame-argument-length *val*) arity+1)
            (let ((env (activation-frame-argument *val* 0)))
              (listify! *val* 1)
              (if (reified-environment? env)
                  (let* ((names (activation-frame-argument *val* 1))
                         (len (- (activation-frame-argument-length *val*)
                                 2))
                         (r (reified-environment-r env))
                         (sr (reified-environment-sr env))
                         (frame (allocate-activation-frame (length names))))
                    (set-activation-frame-next! frame sr)
                    (define (init-frame i)
                      (if (>= i 0)
                          (begin (set-activation-frame-argument!
                                   frame i undefined-value)
                                 (init-frame (- i 1)))))
                    (init-frame (- len 1))
                    (if (not (every? symbol? names))
                        (signal-exception #f (list "Incorrect variable names" names)))
                    (set! *val* (make-reified-environment
                                  frame
                                  (checked-r-extend* r names)))
                    (set! *pc* stack-pop))
                  (signal-exception #t (list "Not an environment" env))))
            (signal-exception #t (list "Incorrect arity" 'enrich)))))))

(define (checked-r-extend* r n*)
  (let ((old-r (bury-r r 1)))
    (define (scan n* i)
      (cond ((pair? n*) (cons (list (car n*) `(checked-local 0 . ,i))
                              (scan (cdr n*) (+ i 1))))
            ((null? n*) (old-r))))
    (scan n* 0)))

(definitial 'variable-value
  (let* ((arity 2)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering variable-value")
        (if (= (activation-frame-argument-length *val*) arity+1)
            (let ((name (activation-frame-argument *val* 0))
                  (env (activation-frame-argument *val* 1)))
              (if (reified-environment? env)
                  (if (symbol? name)
                      (let* ((r (reified-environment-r env))
                             (sr (reified-environment-sr env))
                             (kind
                               (or (let ((var (assq name r)))
                                     (and (pair? var) (cadr var)))
                                   (global-variable? g.current name)
                                   (global-variable? g.init name))))
                        (variable-value-lookup kind sr)
                        (set! *pc* (stack-pop)))
                      (signal-exception #f (list "Not a variable name" name)))
                  (signal-exception #t (list "Not an environment" env))))
            (signal-exception #t (list "Incorrect arity" 'variable-value)))))))

(definitial 'set-variable-value!
  (let* ((arity 3)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering set-variable-value!")
        (if (= (activation-frame-argument-length *val*) arity+1)
            (let ((name (activation-frame-argument *val* 0))
                  (env (activation-frame-argument *val* 1))
                  (v (activation-frame-argument *val* 2)))
              (if (reified-environment? env)
                  (if (symbol? name)
                      (let* ((r (reified-environment-r env))
                             (sr (reified-environment-sr env))
                             (kind
                               (or (let ((var (assq name r)))
                                     (and (pair? var) (cadr var)))
                                   (global-variable? g.current name)
                                   (global-variable? g.init name))))
                        (variable-value-update! kind sr v)
                        (set! *pc* (stack-pop)))
                      (signal-exception #f (list "Not a variable name" name)))
                  (signal-exception #t (list "Not an environment" env))))
            (signal-exception #t (list "Incorrect arity" 'set-variable-value!)))))))

(definitial 'variable-defined?
  (let* ((arity 2)
         (arity+1 (+ arity 1)))
    (make-primitive
      (lambda ()
        (println "entering variable-defined?")
        (if (= (activation-frame-argument-length *val*) arity+1)
            (let ((name (activation-frame-argument *val* 0))
                  (env (activation-frame-argument *val* 1)))
              (if (reified-environment? env)
                  (if (symbol? name)
                      (let ((r (reified-environment-r env))
                            (sr (reified-environment-sr env)))
                        (set! *val*
                              (if (or (let ((var (assq name r)
                                              (and (pair? var) (cadr var))))
                                        (global-variable? g.current name)
                                        (global-variable? g.init name)))
                                  #t #f))
                        (set! *pc* (stack-pop)))
                      (signal-exception #f (list "Not a variable name" name)))
                  (signal-exception #t (list "Not an environment" env))))
            (signal-exception #t (list "Incorrect arity" 'variable-defined?)))))))
