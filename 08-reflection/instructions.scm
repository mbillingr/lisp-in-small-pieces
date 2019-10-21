
(define (run)
  (println "->" *pc* "  OP:" (instruction-decode *code* *pc*))
  (let ((instruction (fetch-byte)))
    ((vector-ref instruction-body-table instruction)))
  (if (not *exit*)  ; workaround for interpreter without call/cc
      (run)
      *val*))

(define (instruction-size code pc)
  (let ((instruction (vector-ref code pc)))
    (vector-ref instruction-size-table instruction)))

(define (instruction-decode code pc)
  (define (decode-fetch-byte)
    (let ((byte (vector-ref code pc)))
      (set! pc (+ pc 1))
      byte))
  (let ((instruction (decode-fetch-byte)))
    ((vector-ref instruction-decode-table instruction) decode-fetch-byte)))

(define (fetch-byte)
  (let ((byte (vector-ref *code* *pc*)))
    (set! *pc* (+ *pc* 1))
    byte))

(define GLOBAL-REF-code 7)
(define CHECKED-GLOBAL-REF-code 8)
(define CONSTANT-code 9)
(define SET-GLOBAL!-code 27)
(define DYNAMIC-REF-code 240)
(define DYNAMIC-PUSH-code 242)

(define instruction-body-table (make-vector 256))
(define instruction-size-table (make-vector 256))
(define instruction-decode-table (make-vector 256))

(define-syntax run-clause
  (syntax-rules ()
    ((run-clause () body) (begin . body))
    ((run-clause (a) body) (let ((a (fetch-byte)))
                             . body))
    ((run-clause (a b) body) (let ((a (fetch-byte))
                                   (b (fetch-byte)))
                               . body))))


(define-syntax size-clause
  (syntax-rules ()
    ((size-clause ())    1)
    ((size-clause (a))   2)
    ((size-clause (a b)) 3)))

(define-syntax decode-clause
  (syntax-rules ()
    ((decode-clause fetcher iname ()) '(iname))
    ((decode-clause fetcher iname (a)) (let ((a (fetcher)))
                                          (list 'iname a)))
    ((decode-clause fetcher iname (a b)) (let ((a (fetcher))
                                               (b (fetcher)))
                                             (list 'iname a b)))))

(define-syntax define-instruction
  (syntax-rules ()
    ((define-instruction (name . args) n . body)
     (begin
       (vector-set! instruction-body-table n (lambda () (run-clause args body)))
       (vector-set! instruction-size-table n (size-clause args))
       (vector-set! instruction-decode-table n (lambda (fetcher) (decode-clause fetcher name args)))))))

(define-instruction (SHALLOW-ARGUMENT-REF0) 1
  (set! *val* (activation-frame-argument *env* 0)))
(define-instruction (SHALLOW-ARGUMENT-REF1) 2
  (set! *val* (activation-frame-argument *env* 1)))
(define-instruction (SHALLOW-ARGUMENT-REF2) 3
  (set! *val* (activation-frame-argument *env* 2)))
(define-instruction (SHALLOW-ARGUMENT-REF3) 4
  (set! *val* (activation-frame-argument *env* 3)))
(define-instruction (SHALLOW-ARGUMENT-REF j) 5
  (set! *val* (activation-frame-argument *env* j)))
(define-instruction (DEEP-ARGUMENT-REF i j) 6
  (set! *val* (deep-fetch *env* i j)))
(define-instruction (GLOBAL-REF i) GLOBAL-REF-code
  (set! *val* (global-fetch i)))
(define-instruction (CHECKED-GLOBAL-REF i) CHECKED-GLOBAL-REF-code
  (set! *val* (global-fetch i))
  (if (eq? *val* undefined-value)
      (signal-exception #t (list "Uninitialized global variable" i))))
(define-instruction (CONSTANT i) CONSTANT-code (set! *val* (quotation-fetch i)))
(define-instruction (PREDEFINED0) 10 (set! *val* #t))
(define-instruction (PREDEFINED1) 11 (set! *val* #f))
(define-instruction (PREDEFINED2) 12 (set! *val* '()))
(define-instruction (PREDEFINED3) 13 (set! *val* (predefined-fetch 3)))
(define-instruction (PREDEFINED4) 14 (set! *val* (predefined-fetch 4)))
(define-instruction (PREDEFINED5) 15 (set! *val* (predefined-fetch 5)))
(define-instruction (PREDEFINED6) 16 (set! *val* (predefined-fetch 6)))
(define-instruction (PREDEFINED7) 17 (set! *val* (predefined-fetch 7)))
(define-instruction (PREDEFINED8) 18 (set! *val* (predefined-fetch 8)))
(define-instruction (PREDEFINED i) 19 (set! *val* (predefined-fetch i)))
(define-instruction (FINISH) 20
  ;(*exit* *val*))
  (set! *exit* #t))
(define-instruction (SET-SHALLOW-ARGUMENT!0) 21
  (set-activation-frame-argument! *env* 0 *val*))
(define-instruction (SET-SHALLOW-ARGUMENT!1) 22
  (set-activation-frame-argument! *env* 1 *val*))
(define-instruction (SET-SHALLOW-ARGUMENT!2) 23
  (set-activation-frame-argument! *env* 2 *val*))
(define-instruction (SET-SHALLOW-ARGUMENT!3) 24
  (set-activation-frame-argument! *env* 3 *val*))
(define-instruction (SET-SHALLOW-ARGUMENT! j) 25
  (set-activation-frame-argument! *env* j *val*))
(define-instruction (SET-DEEP-ARGUMENT! i j) 26
  (deep-update! *env* i j *val*))
(define-instruction (SET-GLOBAL! i) SET-GLOBAL!-code
  (global-update! i *val*))

(define-instruction (LONG-GOTO offset1 offset2) 28
  (let ((offset (+ offset1 (* 256 offset2))))
    (set! *pc* (+ *pc* offset))))
(define-instruction (LONG-JUMP-FALSE offset1 offset2) 29
  (let ((offset (+ offset1 (* 256 offset2))))
    (if (not *val*) (set! *pc* (+ *pc* offset)))))
(define-instruction (SHORT-GOTO offset) 30
  (set! *pc* (+ *pc* offset)))
(define-instruction (SHORT-JUMP-FALSE offset) 31
  (if (not *val*) (set! *pc* (+ *pc* offset))))
(define-instruction (EXTEND-ENV) 32
  (set! *env* (sr-extend* *env* *val*)))
(define-instruction (UNLINK-ENV) 33
  (set! *env* (activation-frame-next *env*)))
(define-instruction (PUSH-VALUE) 34
  (stack-push *val*))
(define-instruction (POP-ARG1) 35
  (set! *arg1* (stack-pop)))
(define-instruction (POP-ARG2) 36
  (set! *arg2* (stack-pop)))
(define-instruction (PRESERVE-ENV) 37
  (preserve-environment))
(define-instruction (RESTORE-ENV) 38
  (restore-environment))
(define-instruction (POP-FUNCTION) 39
  (set! *fun* (stack-pop)))
(define-instruction (CREATE-CLOSURE offset) 40
  (set! *val* (make-closure (+ *pc* offset) *env*)))

(define-instruction (RETURN) 43
  (set! *pc* (stack-pop)))
(define-instruction (PACK-FRAME! arity) 44
  (listify! *val* arity))
(define-instruction (FUNCTION-INVOKE) 45
  (invoke *fun* #f))
(define-instruction (FUNCTION-GOTO) 46
  (invoke *fun* #t))
(define-instruction (POP-CONS-FRAME! arity) 47
  (set-activation-frame-argument!
    *val* arity (cons (stack-pop)
                      (activation-frame-argument *val* arity))))

(define-instruction (ALLOCATE-FRAME1) 50
  (set! *val* (allocate-activation-frame 1)))
(define-instruction (ALLOCATE-FRAME2) 51
  (set! *val* (allocate-activation-frame 2)))
(define-instruction (ALLOCATE-FRAME3) 52
  (set! *val* (allocate-activation-frame 3)))
(define-instruction (ALLOCATE-FRAME4) 53
  (set! *val* (allocate-activation-frame 4)))
(define-instruction (ALLOCATE-FRAME5) 54
  (set! *val* (allocate-activation-frame 5)))
(define-instruction (ALLOCATE-FRAME size+1) 55
  (set! *val* (allocate-activation-frame size+1)))
(define-instruction (ALLOCATE-DOTTED-FRAME arity+1) 56
  (let ((v* (allocate-activation-frame arity+1)))
    (set-activation-frame-argument! v* (- arity+1 1) '())
    (set! *val* v*)))

(define-instruction (POP-FRAME!0) 60
  (set-activation-frame-argument! *val* 0 (stack-pop)))
(define-instruction (POP-FRAME!1) 61
  (set-activation-frame-argument! *val* 1 (stack-pop)))
(define-instruction (POP-FRAME!2) 62
  (set-activation-frame-argument! *val* 2 (stack-pop)))
(define-instruction (POP-FRAME!3) 63
  (set-activation-frame-argument! *val* 3 (stack-pop)))
(define-instruction (POP-FRAME! rank) 64
  (set-activation-frame-argument! *val* rank (stack-pop)))

(define-instruction (ARITY=?1) 71
  (if (not (= (activation-frame-argument-length *val*) 1))
      (signal-exception #f (list "Incorrect arity for nullary function"))))
(define-instruction (ARITY=?2) 72
  (if (not (= (activation-frame-argument-length *val*) 2))
      (signal-exception #f (list "Incorrect arity for unary function"))))
(define-instruction (ARITY=?3) 73
  (if (not (= (activation-frame-argument-length *val*) 3))
      (signal-exception #f (list "Incorrect arity for binary function"))))
(define-instruction (ARITY=?4) 74
  (if (not (= (activation-frame-argument-length *val*) 4))
      (signal-exception #f (list "Incorrect arity for ternary function"))))
(define-instruction (ARITY=? arity+1) 75
  (if (not (= (activation-frame-argument-length *val*) arity+1))
      (signal-exception #f (list "Incorrect arity"))))

(define-instruction (ARITY>=? arity+1) 78
  (if (not (>= (activation-frame-argument-length *val*) arity+1))
      (signal-exception #f (list "Too few function arguments"))))
(define-instruction (SHORT-NUMBER value) 79 (set! *val* value))
(define-instruction (CONSTANT-1) 80 (set! *val* -1))
(define-instruction (CONSTANT0) 81 (set! *val* 0))
(define-instruction (CONSTANT1) 82 (set! *val* 1))
(define-instruction (CONSTANT2) 83 (set! *val* 2))
(define-instruction (CONSTANT4) 84 (set! *val* 4))

(define-instruction (CALL0-newline) 89
  (newline))

(define-instruction (CALL1-car) 90
  (set! *val* (car *val*)))
(define-instruction (CALL1-cdr) 91
  (set! *val* (cdr *val*)))
(define-instruction (CALL1-pair?) 92
  (set! *val* (pair? *val*)))
(define-instruction (CALL1-symbol?) 93
  (set! *val* (symbol? *val*)))
(define-instruction (CALL1-display) 94
  (display *val*))
(define-instruction (CALL1-null?) 95
  (set! *val* (null? *val*)))

(define-instruction (CALL2-cons) 100
  (set! *val* (cons *arg1* *val*)))

(define-instruction (CALL2-eq?) 101
  (set! *val* (eq? *arg1* *val*)))

(define-instruction (CALL2-set-car!) 102
  (set! *val* (set-car! *arg1* *val*)))

(define-instruction (CALL2-set-cdr!) 103
  (set! *val* (set-cdr! *arg1* *val*)))

(define-instruction (CALL2-=) 104
  (set! *val* (= *arg1* *val*)))
(define-instruction (CALL2-<) 105
  (set! *val* (< *arg1* *val*)))
(define-instruction (CALL2-<=) 106
  (set! *val* (<= *arg1* *val*)))
(define-instruction (CALL2->) 107
  (set! *val* (> *arg1* *val*)))
(define-instruction (CALL2->=) 108
  (set! *val* (>= *arg1* *val*)))
(define-instruction (CALL2-+) 109
  (set! *val* (+ *arg1* *val*)))
(define-instruction (CALL2--) 110
  (set! *val* (- *arg1* *val*)))
(define-instruction (CALL2-*) 111
  (set! *val* (* *arg1* *val*)))
(define-instruction (CALL2-/) 112
  (set! *val* (/ *arg1* *val*)))

(define-instruction (DYNAMIC-REF index) DYNAMIC-REF-code
  (set! *val* (find-dynamic-value index)))
(define-instruction (DYNAMIC-POP) 241
  (pop-dynamic-binding))
(define-instruction (DYNAMIC-PUSH index) DYNAMIC-PUSH-code
  (push-dynamic-binding index *val*))

(define-instruction (NON-CONT-ERR) 245
  (signal-exception #f (list "Attempt to continue non-continuable exception")))
(define-instruction (PUSH-HANDLER) 246
  (push-exception-handler))
(define-instruction (POP-HANDLER) 247
  (pop-exception-handler))

(define-instruction (PUSH-ESCAPER offset) 251
  (preserve-environment)
  (let* ((escape (make-escape (+ *stack-index* 3)))
         (frame (allocate-activation-frame 1)))
    (set-activation-frame-argument! frame 0 escape)
    (set! *env* (sr-extend* *env* frame))
    (stack-push escape)
    (stack-push escape-tag)
    (stack-push (+ *pc* offset))))

; ============================================================================

; ---- environment manipulation

(define (quotation-fetch i)
  (vector-ref *constants* i))

; ---- stack manipulation

(define (stack-push v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)))

(define (stack-pop)
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*))

(define (preserve-environment)
  (stack-push *env*))

(define (restore-environment)
  (set! *env* (stack-pop)))

; ---- dynamic variables

(define dynenv-tag (list '*dynenv*))

(define (search-dynenv-index)
  (define (search i)
    (if (< i 0)
        i
        (if (eq? (vector-ref *stack* i) dynenv-tag)
            (- i 1)
            (search (- i 1)))))
  (search (- *stack-index* 1)))

(define (pop-dynamic-binding)
  (stack-pop) (stack-pop) (stack-pop) (stack-pop))

(define (push-dynamic-binding index value)
  (stack-push (search-dynenv-index))
  (stack-push value)
  (stack-push index)
  (stack-push dynenv-tag))

(define (find-dynamic-value index)
  (define (scan i)
    (if (< i 0)
        '*uninit*
        (if (eq? vector-ref *stack* i) index
            (vector-ref *stack* (- i 1))
            (scan (vector-ref *stack* (- i 2))))))
  (scan (search-dynenv-index)))

; ---- exceptions

(define (search-exception-handlers)
  (find-dynamic-value 0))

(define (push-exception-handler)
  (let ((handlers (search-exception-handlers)))
    (push-dynamic-binding 0 (cons *val* handlers))))

(define (pop-exception-handler)
  (pop-dynamic-binding))

(define (signal-exception continuable? exception)
  (let ((handlers (search-exception-handlers))
        (v* (allocate-activation-frame (+ 2 1))))
    (set-activation-frame-argument! v* 0 continuable?)
    (set-activation-frame-argument! v* 1 exception)
    (set! *val* v*)
    (stack-push *pc*)
    (preserve-environment)
    (push-dynamic-binding 0 (if (null? (cdr handlers))
                                handlers
                                (cdr handlers)))
    (if continuable?
        (stack-push 2)    ; pc for (POP-HANDLER) (RESTORE-ENV) (RETURN)
        (stack-push 0))   ; pc for (NON-CONT-ERR)
    (invoke (car handlers) #t)))

; ========  Chapter 8 =======

(define-instruction (CHECKED-DEEP-REF i j) 253
  (set! *val* (deep-fetch *env* i j))
  (if (eq? *val* undefined-value)
      (signal-exception #t (list "Uninitialized local variable"))))

(define-instruction (CREATE-1ST-CLASS-ENV) 254
  (create-first-class-environment *val* *env*))

(define (create-first-class-environment r sr)
  (set! *val* (make-reified-environment sr r)))
