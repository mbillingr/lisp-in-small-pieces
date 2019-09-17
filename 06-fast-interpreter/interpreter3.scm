(import (builtin core)
        (libs utils)
        (libs book)
        (06-fast-interpreter common))

(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?))
      (case (car e)
        ((quote)   (meaning-quotation (cadr e) r tail?))
        ((lambda)  (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)      (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)   (meaning-sequence (cdr e) r tail?))
        ((set!)    (meaning-assignment (cadr e) (caddr e) r tail?))
        (else      (meaning-application (car e) (cdr e) r tail?)))))


(define (make-closure code closed-environment)
  (list 'closure code closed-environment))

(define (closure? obj)
  (eq? (car obj) 'closure))

(define (closure-code obj)
  (cadr obj))

(define (closure-closed-environment obj)
  (caddr obj))

(define (invoke f v*)
  (if (closure? f)
      ((closure-code f) v* (closure-closed-environment f))
      (wrong "Not a function" f)))


(define (meaning-quotation v r tail?)
  (CONSTANT v))

(define (CONSTANT value)
  (lambda () value))


(define (meaning-reference n r tail?)
  (let ((kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local) (let ((i (cadr kind))
                         (j (cddr kind)))
                     (if (= i 0)
                         (SHALLOW-ARGUMENT-REF j)
                         (DEEP-ARGUMENT-REF i j))))
          ((global) (let ((i (cdr kind)))
                      (CHECKED-GLOBAL-REF i)))
          ((predefined) (let ((i (cdr kind)))
                          (PREDEFINED i))))
        (static-wrong "No such variable" n))))

(define (SHALLOW-ARGUMENT-REF j)
  (lambda () (activation-frame-argument *env* j)))

(define (PREDEFINED i)
  (lambda () (predefined-fetch i)))

(define (DEEP-ARGUMENT-REF i j)
  (lambda () (deep-fetch *env* i j)))

(define (GLOBAL-REF i)
  (lambda () (global-fetch i)))

(define (CHECKED-GLOBAL-REF i)
  (lambda ()
    (let ((v (global-fetch i)))
      (if (eq? v undefined-value)
          (wrong "Uninitialized variable")
          v))))


(define (meaning-alternative e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r #f))
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)))
    (ALTERNATIVE m1 m2 m3)))

(define (ALTERNATIVE m1 m2 m3)
  (lambda () (if (m1) (m2) (m3))))


(define (meaning-assignment n e r tail?)
  (let ((m (meaning e r #f))
        (kind (compute-kind r n)))
    (if kind
        (case (car kind)
          ((local) (let ((i (cadr kind))
                         (j (cddr kind)))
                     (if (= i 0)
                         (SHALLOW-ARGUMENT-SET! j m)
                         (DEEP-ARGUMENT-SET! i j m))))
          ((global) (let ((i (cdr kind)))
                      (GLOBAL-SET! i m)))
          ((predefined) (static-wrong "Immutable predefined variable" n)))
        (static-wrong "No such variable" n))))

(define (SHALLOW-ARGUMENT-SET! j m)
  (lambda () (set-activation-frame-argument! *env* j (m))))

(define (DEEP-ARGUMENT-SET! i j m)
  (lambda () (deep-update! *env* i j (m))))

(define (GLOBAL-SET! i m)
  (lambda () (global-update! i (m))))


(define (meaning-sequence e+ r tail?)
  (if (pair? e+)
      (if (pair? (cdr e+))
          (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
          (meaning*-single-sequence (car e+) r tail?))
      (static-wrong "Illegal syntax (begin)")))

(define (meaning*-single-sequence e r tail?)
  (meaning e r tail?))

(define (meaning*-multiple-sequence e e+ r tail?)
  (let ((m1 (meaning e r #f))
        (m+ (meaning-sequence e+ r tail?)))
    (SEQUENCE m1 m+)))

(define (SEQUENCE m m+)
  (lambda () (m) (m+)))


(define (meaning-abstraction nn* e+ r tail?)
  (define (parse n* regular)
    (cond
      ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
      ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
      (else       (meaning-dotted-abstraction
                    (reverse regular) n* e+ r tail?))))
  (parse nn* '()))

(define (meaning-fix-abstraction n* e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence e+ r2 #t)))
    (FIX-CLOSURE m+ arity)))

(define (meaning-dotted-abstraction n* n e+ r tail?)
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 #t)))
    (NARY-CLOSURE m+ arity)))

(define (FIX-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (= (activation-frame-argument-length v*) arity+1)
            (begin (set! *env* (sr-extend* sr v*))
                   (m+))
            (wrong "Incorrect arity")))
      (make-closure the-function *env*))))

(define (NARY-CLOSURE m+ arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (define (the-function v* sr)
        (if (>= (activation-frame-argument-length v*) arity+1)
            (begin
              (listify! v* arity)
              (set! *env* (sr-extend* sr v*))
              (m+))
            (wrong "Incorrect arity")))
      (make-closure the-function *env*))))

(define (meaning-application e e* r tail?)
  (cond ((and (symbol? e)
              (let ((kind (compute-kind r e)))
                (and (pair? kind)
                     (eq? 'predefined (car kind))
                     (let ((desc (get-description e)))
                       (and desc
                            (eq? 'function (car desc))
                            (or (= (length (cddr desc)) (length e*))
                                (static-wrong
                                  "Incorrect arity for primitive" e)))))))
         (meaning-primitive-application e e* r tail?))
        ((and (pair? e)
              (eq? 'lambda (car e)))
         (meaning-closed-application e e* r tail?))
        (else (meaning-regular-application e e* r tail?))))

(define (meaning-regular-application e e* r tail?)
  (let* ((m (meaning e r #f))
         (m* (meaning* e* r (length e*) #f)))
    (if tail? (TR-REGULAR-CALL m m*)
              (REGULAR-CALL m m*))))

(define (meaning* e* r size tail?)
  (if (pair? e*)
      (meaning-some-arguments (car e*) (cdr e*) r size tail?)
      (meaning-no-argument r size tail?)))

(define (meaning-some-arguments e e* r size tail?)
  (let ((m (meaning e r #f))
        (m* (meaning* e* r size tail?))
        (rank (- size (+ (length e*) 1))))
    (STORE-ARGUMENT m m* rank)))

(define (meaning-no-argument r size tail?)
  (ALLOCATE-FRAME size))

(define (TR-REGULAR-CALL m m*)
  (lambda ()
    (let ((f (m)))
      (invoke f (m*)))))

(define (REGULAR-CALL m m*)
  (lambda ()
    (let* ((f (m))
           (v* (m*))
           (sr *env*)
           (result (invoke f v*)))
      (set! *env* sr)
      result)))

(define (STORE-ARGUMENT m m* rank)
  (lambda ()
    (let* ((v (m))
           (v* (m*)))
      (set-activation-frame-argument! v* rank v)
      v*)))

(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (lambda ()
      (allocate-activation-frame size+1))))


(define (meaning-closed-application e ee* r tail?)
  (let ((nn* (cadr e)))
    (define (parse n* e* regular)
      (cond ((pair? n*)
             (if (pair? e*)
                 (parse (cdr n*) (cdr e*) (cons (car n*) regular))
                 (static-wrong "Too few arguments" e ee*)))
            ((null? n*)
             (if (null? e*)
                 (meaning-fix-closed-application nn* (cddr e) ee* r tail?)
                 (static-wrong "Too many arguments" e ee*)))
            (else (meaning-dotted-closed-application
                    (reverse regular) n* (cddr e) ee* r tail?))))
    (parse nn* ee* '())))

(define (meaning-fix-closed-application n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) #f))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail? (TR-FIX-LET m* m+)
              (FIX-LET m* m+))))

(define (meaning-dotted-closed-application n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)))
    (if tail? (TR-FIX-LET m* m+)
              (FIX-LET m* m+))))

(define (meaning-dotted* e* r size arity tail?)
  (if (pair? e*)
      (meaning-some-dotted-arguments (car e*) (cdr e*)
                                     r size arity tail?)
      (meaning-no-dotted-argument r size arity tail?)))

(define (meaning-some-dotted-arguments e e* r size arity tail?)
  (let ((m (meaning e r #f))
        (m* (meaning-dotted* e* r size arity tail?))
        (rank (- size (+ (length e*) 1))))
    (if (< rank arity)
        (STORE-ARGUMENT m m* rank)
        (CONS-ARGUMENT m m* arity))))

(define (meaning-no-dotted-argument r size arity tail?)
  (ALLOCATE-DOTTED-FRAME arity))

(define (FIX-LET m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (let ((result (m+)))
      (set! *env* (environment-next *env*))
      result)))

(define (TR-FIX-LET m* m+)
  (lambda ()
    (set! *env* (sr-extend* *env* (m*)))
    (m+)))

(define (CONS-ARGUMENT m m* arity)
  (lambda ()
    (let* ((v (m))
           (v* (m*)))
      (set-activation-frame-argument!
        v* arity (cons v (activation-frame-argument v* arity)))
      v*)))

(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (lambda ()
      (let ((v* (allocate-activation-frame arity+1)))
        (set-activation-frame-argument! v* arity '())
        v*))))


(define (meaning-primitive-application e e* r tail?)
  (let* ((desc (get-description e))
         (address (cadr desc))
         (size (length e*)))
    (case size
      ((0) (CALL0 address))
      ((1) (let ((m1 (meaning (car e*) r #f)))
             (CALL1 address m1)))
      ((2) (let ((m1 (meaning (car e*) r #f))
                 (m2 (meaning (cadr e*) r #f)))
             (CALL2 address m1 m2)))
      ((3) (let ((m1 (meaning (car e*) r #f))
                 (m2 (meaning (cadr e*) r #f))
                 (m3 (meaning (caddr e*) r #f)))
             (CALL3 address m1 m2 m3)))
      (else (meaning-regular-application e e* r tail?)))))

(define (CALL0 address)
  (lambda () (address)))

(define (CALL3 address m1)
  (lambda () (address (m1))))

(define (CALL2 address m1 m2)
  (lambda () (let* ((v1 (m1)))
               (address v1 (m2)))))

(define (CALL3 address m1 m2 m3)
  (lambda () (let* ((v1 (m1))
                    (v2 (m2)))
               (address v1 v2 (m3)))))


(define (defprimitive name value arity)
  (case arity
    ((0) (defprimitive0 name value))
    ((1) (defprimitive1 name value))
    ((2) (defprimitive2 name value))
    ((3) (defprimitive3 name value))
    (else static-wrong "Unsupported primitive arity" name arity)))

(define (defprimitive0 name value)
  (definitial name
    (let* ((arity+1 (+ 0 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value)
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value))
      (make-closure behavior sr.init))))

(define (defprimitive1 name value)
  (definitial name
    (let* ((arity+1 (+ 1 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value (activation-frame-argument v* 0))
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value a))
      (make-closure behavior sr.init))))

(define (defprimitive2 name value)
  (definitial name
    (let* ((arity+1 (+ 2 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value (activation-frame-argument v* 0)
                          (activation-frame-argument v* 1))
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value a b))
      (make-closure behavior sr.init))))

(define (defprimitive3 name value)
  (definitial name
    (let* ((arity+1 (+ 3 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value (activation-frame-argument v* 0)
                          (activation-frame-argument v* 1)
                          (activation-frame-argument v* 2))
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value a b c))
      (make-closure behavior sr.init))))


(define *env* sr.init)

(define (chapter63-interpreter)
  (define (toplevel)
    (set! *env* sr.init)
    (display ((meaning (read) r.init #t)))
    (toplevel))
  (toplevel))


(definitial 't #t)
(definitial 'f #f)
(definitial 'nil '())
(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defprimitive 'eq? eq? 2)
(defprimitive '= = 2)
(defprimitive '< < 2)
(defprimitive '<= <= 2)
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defprimitive '* * 2)
(defprimitive '/ / 2)
; omitted definition of apply (section 6.3.14) because my interpreter lacks
; the form (do) and because of lazyness.          


(chapter63-interpreter)
