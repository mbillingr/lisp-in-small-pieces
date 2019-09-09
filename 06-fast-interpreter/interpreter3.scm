(import (builtin core)
        (libs utils)
        (libs book))

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
  (eq? (car obj 'closure)))

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
  (lambda () (set-activation-frame-argument *env* j (m))))

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
                    (reverse regular) n* e+ tail?))))
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
