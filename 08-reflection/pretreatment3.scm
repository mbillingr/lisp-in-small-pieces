
(define (meaning e r tail?)
  (if (atom? e)
      (if (symbol? e) (meaning-reference e r tail?)
                      (meaning-quotation e r tail?))
      (case (car e)
        ((quote)       (meaning-quotation (cadr e) r tail?))
        ((lambda)      (meaning-abstraction (cadr e) (cddr e) r tail?))
        ((if)          (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
        ((begin)       (meaning-sequence (cdr e) r tail?))
        ((set!)        (meaning-assignment (cadr e) (caddr e) r tail?))
        ((dynamic)     (meaning-dynamic-reference (cadr e) r tail?))
        ((dynamic-let) (meaning-dynamic-let (car (cadr e))
                                            (cadr (cadr e))
                                            (cddr e) r tail?))
        ((bind-exit)   (meaning-bind-exit (caadr e) (cddr e) r tail?))
        ((monitor)     (meaning-monitor (cadr e) (cddr e) r tail?))
        (else          (meaning-application (car e) (cdr e) r tail?)))))


(define (meaning-quotation v r tail?)
  (CONSTANT v))


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


(define (meaning-alternative e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r #f))
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)))
    (ALTERNATIVE m1 m2 m3)))


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

(define (meaning-dynamic-let n e e+ r tail?)
  (let ((index (get-dynamic-variable-index n))
        (m (meaning e r #f))
        (m+ (meaning-sequence e+ r #f)))
    (append m (DYNAMIC-PUSH index) m+ (DYNAMIC-POP))))

(define (meaning-dynamic-reference n r tail?)
  (let ((index (get-dynamic-variable-index n)))
    (DYNAMIC-REF index)))

(define (meaning-monitor e e+ r tail?)
  (let ((m (meaning e r #f))
        (m+ (meaning-sequence e+ r #f)))
    (append m (PUSH-HANDLER) m+ (POP-HANDLER))))

(define (meaning-bind-exit n e+ r tail?)
  (let* ((r2 (r-extend* r (list n)))
         (m+ (meaning-sequence e+ r2 #t)))
    (ESCAPER m+)))

(define (SHALLOW-ARGUMENT-SET! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)))

(define (DEEP-ARGUMENT-SET! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)))

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i))) 
