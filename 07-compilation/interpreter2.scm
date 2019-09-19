(import (builtin core)
        (libs utils)
        (libs book)
        (06-fast-interpreter common))

; the instructions from interpreter1 are further linearized and put in sequence

(include "pretreatment.scm")


(define (make-closure code closed-environment)
  (list 'closure code closed-environment))

(define (closure? obj)
  (eq? (car obj) 'closure))

(define (closure-code obj)
  (cadr obj))

(define (closure-closed-environment obj)
  (caddr obj))

(define (invoke f)
  (if (closure? f)
      ((closure-code f) (closure-closed-environment f))
      (wrong "Not a function" f)))


(define (CONSTANT value)
  (list (lambda () (set! *val* value))))

;(define (SHALLOW-ARGUMENT-REF j)
;  (lambda () (set! *val* (activation-frame-argument *env* j))))

;(define (PREDEFINED i)
;  (lambda () (set! *val* (predefined-fetch i))))

;(define (DEEP-ARGUMENT-REF i j)
;  (lambda () (set! *val* (deep-fetch *env* i j))))

;(define (GLOBAL-REF i)
;  (lambda () (set! *val* (global-fetch i))))

;(define (CHECKED-GLOBAL-REF i)
;  (lambda ()
;    (let ((v (global-fetch i)))
;      (if (eq? v undefined-value)
;          (wrong "Uninitialized variable")
;          (set! *val* v))))


(define (ALTERNATIVE m1 m2 m3)
  (append m1
          (JUMP-FALSE (+ 1 (length m2)))
          m2
          (GOTO (length m3))
          m3))

(define (JUMP-FALSE i)
  (list (lambda () (if (not *val*)
                       (set! *pc* (list-tail *pc* i))))))
(define (GOTO i)
  (list (lambda () (set! *pc* (list-tail *pc* i)))))

(define (SHALLOW-ARGUMENT-SET! j m)
  (append m (SET-SHALLOW-ARGUMENT! j)))
(define (SET-SHALLOW-ARGUMENT! j)
  (list (lambda () (set-activation-frame-argument *env* j *val*))))

;(define (DEEP-ARGUMENT-SET! i j m)
;  (lambda () (m)
;             (deep-update! *env* i j *env*))))

;(define (GLOBAL-SET! i m)
;  (lambda () (m)
;             (global-update! i *val*))))


;(define (SEQUENCE m m+)
;  (lambda () (m) (m+)))


;(define (FIX-CLOSURE m+ arity)
;  (let ((arity+1 (+ arity 1)))
;    (lambda ()
;      (define (the-function sr)
;        (if (= (activation-frame-argument-length *val*) arity+1)
;            (begin (set! *env* (sr-extend* sr *val*))
;                   (m+)
;            (wrong "Incorrect arity"))
;      (set! *val* (make-closure the-function *env*))))

;(define (NARY-CLOSURE m+ arity)
;  (let ((arity+1 (+ arity 1)))
;    (lambda ()
;      (define (the-function sr)
;        (if (>= (activation-frame-argument-length *val*) arity+1)
;            (begin
;              (listify! *val* arity)
;              (set! *env* (sr-extend* sr *val*))
;              (m+)
;            (wrong "Incorrect arity"))
;      (set! *val* (make-closure the-function *env*))))


(define (TR-REGULAR-CALL m m*)
  (append m
          (PUSH-VALUE)
          m*
          (POP-FUNCTION)
          (FUNCTION-INVOKE)))

(define (REGULAR-CALL m m*)
  (append m
          (PUSH-VALUE)
          m*
          (POP-FUNCTION)
          (PRESERVE-ENV)
          (FUNCTION-INVOKE)
          (RESTORE-ENV)))

(define (PUSH-VALUE)
  (list (lambda () (stack-push *val*))))
(define (POP-FUNCTION)
  (list (lambda () (set! *fun* (stack-pop)))))
(define (PRESERVE-ENV)
  (list (lambda () (stack-push *env*))))
(define (RESTORE-ENV)
  (list (lambda () (set! *env* (stack-pop)))))
(define (FUNCTION-INVOKE)
  (list (lambda () (invoke *fun*))))

;(define (STORE-ARGUMENT m m* rank)
;  (lambda () (m)
;             (stack-push *val*)
;             (m*)
;             (let ((v (stack-pop)))
;               (set-activation-frame-argument! *val* rank v))))

;(define (ALLOCATE-FRAME size)
;  (let ((size+1 (+ size 1)))
;    (lambda () (set! *val* (allocate-activation-frame size+1)))))


;(define (FIX-LET m* m+)
;  (lambda () (m*)
;             (set! *env* (sr-extend* *env* *val*))
;             (m+)
;             ;(set! *env* (activation-frame-next *env*))))
;             (set! *env* (environment-next *env*)))))  ; I think this is correct

;(define (TR-FIX-LET m* m+)
;  (lambda () (m*)
;             (set! *env* (sr-extend* *env* *val*))
;             (m+))))

;(define (CONS-ARGUMENT m m* arity)
;  (lambda () (m)
;             (stack-push *val*)
;             (m*)
;             (let ((v (stack-pop)))
;               (set-activation-frame-argument!
;                 *val* arity (cons v (activation-frame-argument *val* arity)))))

;(define (ALLOCATE-DOTTED-FRAME arity)
;  (let ((arity+1 (+ arity 1)))
;    (lambda () (set! *val* (allocate-activation-frame arity+1))
;               (set-activation-frame-argument! *val* arity '()))))


;(define (CALL0 address)
;  (lambda () (set! *val* (address))))

;(define (CALL1 address m1)
;  (lambda () (m1)
;             (set! *val* (address *val*)))))

;(define (CALL2 address m1 m2)
;  (lambda () (m1)
;             (stack-push *val*)
;             (m2)
;             (set! *arg1* (stack-pop))
;             (set! *val* (address *arg1* *val*)))))

;(define (CALL3 address m1 m2 m3)
;  (lambda () (m1)
;             (stack-push *val*)
;             (m2)
;             (stack-push *val*)
;             (m3)
;             (set! *arg2* (stack-pop))
;             (set! *arg1* (stack-pop))
;             (set! *val* (address *arg1* *arg2* *val*)))))


(define (run)
  (if (not (null? *pc*))
      (let ((instruction (car *pc*)))
        (set! *pc* (cdr *pc*))
        (instruction)
        (run))))


; some dummy definitions
(define *pc* '())
(define *env* '())
(define *val* '())
(define *fun* '())
(define *arg1* '())
(define *arg2* '())

(define *stack* (make-vector 1000))
(define *stack-index* 0)

(define (stack-push v)
  (println "push" v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)))

(define (stack-pop)
  (println "pop")
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*))


(include "common-primitives.scm")


(define (chapter7-interpreter1)
  (define (toplevel)
    (set! *env* sr.init)
    (set! *pc* (meaning (read) r.init #t))
    (run)
    (display *val*)
    (toplevel))
  (toplevel))

(chapter7-interpreter1)
