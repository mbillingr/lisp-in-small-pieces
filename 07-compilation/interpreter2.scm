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
  (cond ((closure? f) (stack-push *pc*)
                      (set! *env* (closure-closed-environment f))
                      (set! *pc* (closure-code f)))
        (else (wrong "Not a function" f))))

(define (RETURN)
  (list (lambda () (set! *pc* (stack-pop)))))

(define (CONSTANT value)
  (list (lambda () (set! *val* value))))

(define (SHALLOW-ARGUMENT-REF j)
  (list (lambda () (set! *val* (activation-frame-argument *env* j)))))

(define (PREDEFINED i)
  (list (lambda () (set! *val* (predefined-fetch i)))))

(define (DEEP-ARGUMENT-REF i j)
  (list (lambda () (set! *val* (deep-fetch *env* i j)))))

(define (GLOBAL-REF i)
  (list (lambda () (set! *val* (global-fetch i)))))

(define (CHECKED-GLOBAL-REF i)
  (list (lambda () (set! *val* (global-fetch i))
                   (if (eq? *val* undefined-value)
                       (wrong "Uninitialized variable")))))


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

(define (DEEP-ARGUMENT-SET! i j m)
  (append m (SET-DEEP-ARGUMENT! i j)))

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)))
(define (SET-GLOBAL! i)
  (list (lambda () (global-update! i *val*))))


(define (SEQUENCE m m+)
  (append m m+))


(define (FIX-CLOSURE m+ arity)
  (define the-function
    (append (ARITY=? (+ arity 1))
            (EXTEND-ENV)
            m+
            (RETURN)))
  (append (CREATE-CLOSURE 1)
          (GOTO (length the-function))
          the-function))

(define (NARY-CLOSURE m+ arity)
  (define the-function
    (append (ARITY>=? (+ arity 1))
            (PACK-FRAME! arity)
            (EXTEND-ENV)
            m+
            (RETURN)))
  (append (CREATE-CLOSURE 1)
          (GOTO (length the-function))
          the-function))

(define (CREATE-CLOSURE offset)
  (list (lambda () (set! *val* (make-closure (list-tail *pc* offset) *env*)))))

(define (PACK-FRAME! arity)
  (list (lambda () (listify! *val* arity))))


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

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE)
          m* (POP-FRAME! rank)))

;(define (ALLOCATE-FRAME size)
;  (let ((size+1 (+ size 1)))
;    (lambda () (set! *val* (allocate-activation-frame size+1)))))
(define (ALLOCATE-FRAME size)
  (let ((size+1 (+ size 1)))
    (list (lambda () (set! *val* (allocate-activation-frame size+1))))))


(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)))

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+))

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE)
          m* (POP-CONS-FRAME! arity)))

;(define (ALLOCATE-DOTTED-FRAME arity)
;  (let ((arity+1 (+ arity 1)))
;    (lambda () (set! *val* (allocate-activation-frame arity+1))
;               (set-activation-frame-argument! *val* arity '()))))
(define (ALLOCATE-DOTTED-FRAME arity)
  (let ((arity+1 (+ arity 1)))
    (list (lambda () (set! *val* (allocate-activation-frame arity+1))
                     (set-activation-frame-argument! *val* arity '())))))


;(define (CALL0 address)
;  (lambda () (set! *val* (address))))

(define (CALL1 address m1)
  (append m1 (INVOKE1 address)))

(define (CALL2 address m1 m2)
  (append m1 (PUSH-VALUE) m2 (POP-ARG1) (INVOKE2 address)))

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE)
          m2 (PUSH-VALUE)
          m3 (POP-ARG2) (POP-ARG1)
          (INVOKE3 address)))

(define (ARITY=? arity+1)
  (list (lambda () (if (not (= (activation-frame-argument-length *val*) arity+1))
                       (wrong "Incorrect arity")))))

(define (ARITY>=? arity+1)
  (list (lambda () (if (not (>= (activation-frame-argument-length *val*) arity+1))
                       (wrong "Incorrect arity")))))

(define (EXTEND-ENV)
  (list (lambda() (set! *env* (sr-extend* *env* *val*)))))

(define (POP-FRAME! rank)
  (list (lambda () (set-activation-frame-argument! *val* rank (stack-pop)))))

(define (POP-CONS-FRAME! arity)
  (list (lambda () (set-activation-frame-argument!
                     *val* arity (cons (stack-pop)
                                       (activation-frame-argument *val* arity))))))

(define (POP-ARG1)
  (list (lambda () (set! *arg1* (stack-pop)))))

(define (POP-ARG2)
  (list (lambda () (set! *arg2* (stack-pop)))))

(define (INVOKE1 address)
  (list (lambda () (set! *val* (address *val*)))))

(define (INVOKE2 address)
  (list (lambda () (set! *val* (address *arg1* *val*)))))

(define (INVOKE3 address)
  (list (lambda () (set! *val* (address *arg1* *arg2 *val*)))))

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
