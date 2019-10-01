(import (builtin core)
        (libs utils)
        (libs book))

; instructions are encoded as bytecode
; call/cc is defined as a primitive
; bind-exit is defined as a special form
; dynamic variables with special forms
; exception handling

(include "common-stuff.scm")
(include "pretreatment.scm")

(define backup.g.current g.current)
(define (original.g.current) backup.g.current)

(define *code* 'anything)
(define *pc* 'anything)
(define *constants* 'anything)
(define *env* 'anything)
(define *dynenv* 'anything)
(define *quotations* 'anything)
(define *stack* 'anything)
(define *stack-index* 'anything)
(define *val* 'anything)
(define *fun* 'anything)
(define *arg1* 'anything)
(define *arg2* 'anything)
(define *exit* 'anything)
(define finish-pc 'anything)

; ===========================================================================

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

(define (meaning-monitor e e+ r tail?)
  (let ((m (meaning e r #f))
        (m+ (meaning-sequence e+ r #f)))
    (append m (PUSH-HANDLER) m+ (POP-HANDLER))))

(define (PUSH-HANDLER) (list 246))
(define (POP-HANDLER)  (list 247))
(define (NON-CONT-ERR) (list 245))

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


; ===========================================================================

(define (meaning-dynamic-let n e e+ r tail?)
  (let ((index (get-dynamic-variable-index n))
        (m (meaning e r #f))
        (m+ (meaning-sequence e+ r #f)))
    (append m (DYNAMIC-PUSH index) m+ (DYNAMIC-POP))))

(define (meaning-dynamic-reference n r tail?)
  (let ((index (get-dynamic-variable-index n)))
    (DYNAMIC-REF index)))

(define (DYNAMIC-PUSH index) (list 242 index))
(define (DYNAMIC-POP)        (list 241))
(define (DYNAMIC-REF index)  (list 240 index))

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
  (set! *dynenv* (cdr *dynenv*)))

(define (push-dynamic-binding index value)
  (set! *dynenv* (cons (cons index value) *dynenv*)))

(define *dynamic-variables* '())

(define (get-dynamic-variable-index n)
  (let ((where (memq n *dynamic-variables*)))
    (if where
        (length where)
        (begin
          (set! *dynamic-variables* (cons n *dynamic-variables*))
          (length *dynamic-variables*)))))

(define (find-dynamic-value index)
  (define (scan dynenv)
    (if (pair? dynenv)
        (if (eq? (caar dynenv) index)
            (cdar dynenv)
            (scan (cdr dynenv)))
        '*uninit*))
  (scan *dynenv*))

; ===========================================================================

(define (meaning-bind-exit n e+ r tail?)
  (let* ((r2 (r-extend* r (list n)))
         (m+ (meaning-sequence e+ r2 #t)))
    (ESCAPER m+)))

(define (ESCAPER m+)
  (append (PUSH-ESCAPER (+ 1 (length m+)))
          m+
          (RETURN)
          (POP-ESCAPER)))

(define (POP-ESCAPER) (list 250))
(define escape-tag (list '*ESCAPE*))
(define (PUSH-ESCAPER offset) (list 251 offset))

; instructions defined below

(define (make-escape stack-index)
  `(escape ,stack-index))

(define (escape? obj)
  (and (pair? obj) (eq? (car obj) 'escape)))

(define (escape-stack-index obj)
  (cadr obj))

(define (escape-invoke f tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (if (escape-valid? f)
          (begin (set! *stack-index* (escape-stack-index f))
                 (set! *val* (activation-frame-argument *val* 0))
                 (set! *pc* (stack-pop)))
          (signal-exception #f (list "Escape out of extent" f)))
      (signal-exception #f (list "Incorrect arity" 'escape))))

(define (escape-valid? f)
  (let ((index (escape-stack-index f)))
    (and (>= *stack-index* index)
         (eq? f (vector-ref *stack* (- index 3)))
         (eq? escape-tag (vector-ref *stack* (- index 2))))))


; ===========================================================================
(define (invoke f tail?)
  (cond ((closure? f) (if (not tail?)
                          (stack-push *pc*))
                      (set! *env* (closure-closed-environment f))
                      (set! *pc* (closure-code f)))
        ((primitive? f) (if (not tail?)
                            (stack-push *pc*))
                        ((primitive-address f)))
        ((continuation? f) (continuation-invoke f tail?))
        ((escape? f) (escape-invoke f tail?))
        (else (signal-exception #f (list "Not a function" f)))))

; ===========================================================================

(define (GLOBAL-SET! i m)
  (append m (SET-GLOBAL! i)))

(define (ALTERNATIVE m1 m2 m3)
  (append m1
          (JUMP-FALSE (+ 2 (length m2)))
          m2
          (GOTO (length m3))
          m3))

(define (SEQUENCE m m+)
  (append m m+))

(define (TR-REGULAR-CALL m m*)
  (append m
          (PUSH-VALUE)
          m*
          (POP-FUNCTION)
          (FUNCTION-GOTO)))

(define (REGULAR-CALL m m*)
  (append m
          (PUSH-VALUE)
          m*
          (POP-FUNCTION)
          (PRESERVE-ENV)
          (FUNCTION-INVOKE)
          (RESTORE-ENV)))

(define (STORE-ARGUMENT m m* rank)
  (append m (PUSH-VALUE)
          m* (POP-FRAME! rank)))

(define (CALL0 address)
  (INVOKE0 address))

(define (CALL1 address m1)
  (append m1 (INVOKE1 address)))

(define (CALL2 address m1 m2)
  (append m1 (PUSH-VALUE) m2 (POP-ARG1) (INVOKE2 address)))

(define (CALL3 address m1 m2 m3)
  (append m1 (PUSH-VALUE)
          m2 (PUSH-VALUE)
          m3 (POP-ARG2) (POP-ARG1)
          (INVOKE3 address)))

(define (FIX-CLOSURE m+ arity)
  (define the-function
    (append (ARITY=? (+ arity 1))
            (EXTEND-ENV)
            m+
            (RETURN)))
  (append (CREATE-CLOSURE 2)
          (GOTO (length the-function))
          the-function))

(define (NARY-CLOSURE m+ arity)
  (define the-function
    (append (ARITY>=? (+ arity 1))
            (PACK-FRAME! arity)
            (EXTEND-ENV)
            m+
            (RETURN)))
  (append (CREATE-CLOSURE 2)
          (GOTO (length the-function))
          the-function))

(define (FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+ (UNLINK-ENV)))

(define (TR-FIX-LET m* m+)
  (append m* (EXTEND-ENV) m+))

(define (CONS-ARGUMENT m m* arity)
  (append m (PUSH-VALUE)
          m* (POP-CONS-FRAME! arity)))

; ===========================================================================
(define (run)
  ;(println "->" *pc* "  OP:" (instruction-decode *code* *pc*))
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

(define (check-byte j)
  (if (or (< j 0) (> j 255))
      (static-wrong "Cannot pack this number within a byte" j)))

(define (SHALLOW-ARGUMENT-REF j)
  (check-byte j)
  (case j
    ((0 1 2 3) (list (+ 1 j)))
    (else      (list 5 j))))

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

(define (SET-SHALLOW-ARGUMENT! j)
  (case j
    ((0 1 2 3) (list (+ 21 j)))
    (else      (list 25 j))))

(define-instruction (SET-SHALLOW-ARGUMENT!2) 21
  (set-activation-frame-argument! *env* 0 *val*))

(define-instruction (SET-SHALLOW-ARGUMENT!1) 22
  (set-activation-frame-argument! *env* 1 *val*))

(define-instruction (SET-SHALLOW-ARGUMENT!2) 23
  (set-activation-frame-argument! *env* 2 *val*))

(define-instruction (SET-SHALLOW-ARGUMENT!3) 24
  (set-activation-frame-argument! *env* 3 *val*))

(define-instruction (SET-SHALLOW-ARGUMENT! j) 25
  (set-activation-frame-argument! *env* j *val*))

(define (DEEP-ARGUMENT-REF i j) (list 6 i j))
(define (SET-DEEP-ARGUMENT! i j) (list 26 i j))

(define-instruction (DEEP-ARGUMENT-REF i j) 6
  (set! *val* (deep-fetch *env* i j)))

(define-instruction (SET-DEEP-ARGUMENT! i j) 26
  (deep-update! *env* i j *val*))

(define (GLOBAL-REF i) (list 7 i))
(define (CHECKED-GLOBAL-REF i) (list 8 i))
(define (SET-GLOBAL! i) (list 27 i))

(define-instruction (GLOBAL-REF i) 7
  (set! *val* (global-fetch i)))

(define-instruction (CHECKED-GLOBAL-REF i) 8
  (set! *val* (global-fetch i))
  (if (eq? *val* undefined-value)
      (signal-exception #t (list "Uninitialized global variable" i))))

(define-instruction (SET-GLOBAL! i) 27
  (global-update! i *val*))

(define (PREDEFINED i)
  (check-byte i)
  (case i
    ((0 1 2 3 4 5 6 7 8) (list (+ 10 i)))
    (else                (list 19 i))))

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

(define (CONSTANT value)
  (cond ((eq? value #t)     (list 10))
        ((eq? value #f)     (list 11))
        ((eq? value '())    (list 12))
        ((equal? value -1)  (list 80))
        ((equal? value 0)   (list 81))
        ((equal? value 1)   (list 82))
        ((equal? value 2)   (list 83))
        ((equal? value 4)   (list 84))
        ((and (integer? value)
              (<= 0 value)
              (< value 255))
         (list 79 value))
        (else (EXPLICIT-CONSTANT value))))

(define (EXPLICIT-CONSTANT value)
  (set! *quotations* (append *quotations* (list value)))
  (list 9 (- (length *quotations*) 1)))
(define-instruction (CONSTANT i) 9 (set! *val* (quotation-fetch i)))

(define-instruction (CONSTANT-1) 80 (set! *val* -1))
(define-instruction (CONSTANT0) 81 (set! *val* 0))
(define-instruction (CONSTANT1) 82 (set! *val* 1))
(define-instruction (CONSTANT2) 83 (set! *val* 2))
(define-instruction (CONSTANT4) 84 (set! *val* 4))
(define-instruction (SHORT-NUMBER value) 79 (set! *val* value))

(define (GOTO offset)
  (cond ((< offset 255) (list 30 offset))
        ((< offset (+ 255 (* 255 256)))
         (let ((offset1 (modulo offset 256)))
              ((offset2 (quotient offset 256)))
           (list 28 offset1 offset2)))
        (else (static-wrong "too long jump" offset))))

(define (JUMP-FALSE offset)
  (cond ((< offset 255) (list 31 offset))
        ((< offset (+ 255 (* 255 256)))
         (let ((offset1 (modulo offset 256)))
              ((offset2 (quotient offset 256)))
           (list 29 offset1 offset2)))
        (else (static-wrong "too long jump" offset))))

(define-instruction (SHORT-GOTO offset) 30
  (set! *pc* (+ *pc* offset)))

(define-instruction (SHORT-JUMP-FALSE offset) 31
  (if (not *val*) (set! *pc* (+ *pc* offset))))

(define-instruction (LONG-GOTO offset1 offset2) 28
  (let ((offset (+ offset1 (* 256 offset2))))
    (set! *pc* (+ *pc* offset))))

(define-instruction (LONG-JUMP-FALSE offset1 offset2) 29
  (let ((offset (+ offset1 (* 256 offset2))))
    (if (not *val*) (set! *pc* (+ *pc* offset)))))

(define (ALLOCATE-FRAME size)
  (case size
    ((0 1 2 3 4) (list (+ 50 size)))
    (else        (list 55 (+ size 1)))))

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

(define (ALLOCATE-DOTTED-FRAME arity) (list 56 (+ arity 1)))
(define-instruction (ALLOCATE-DOTTED-FRAME arity+1) 56
  (let ((v* (allocate-activation-frame arity+1)))
    (set-activation-frame-argument! v* (- arity+1 1) '())
    (set! *val* v*)))

(define (POP-FRAME! rank)
  (case rank
    ((0 1 2 3)  (list (+ 60 rank)))
    (else       (list 64 rank))))

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

(define (INVOKE0 address)
  (case address
    (else (static-wrong "Cannot integrate" address))))

(define (INVOKE1 address)
  (case address
    ((car)      (list 90))
    ((cdr)      (list 91))
    ((pair?)    (list 92))
    ((symbol?)  (list 93))
    ((display)  (list 94))
    ((null?)    (list 95))
    (else (static-wrong "Cannot integrate" address))))

(define-instruction (CALL1-car) 90
  (set! *val* (car *val*)))

(define-instruction (CALL1-cdr) 91
  (set! *val* (cdr *val*)))

(define-instruction (CALL1-pair?) 92
  (set! *val* (pair? *val*)))

(define-instruction (CALL1-symbol?) 93
  (set! *val* (symbol? *val*)))

(define-instruction (CALL1-display) 94
  (set! *val* (display *val*)))

(define-instruction (CALL1-null?) 95
  (set! *val* (null? *val*)))

(define (INVOKE2 address)
  (case address
    ((cons)     (list 100))
    ((eq?)      (list 101))
    ((set-car!) (list 102))
    ((set-cdr!) (list 103))
    ((=)        (list 104))
    ((<)        (list 105))
    ((<=)       (list 106))
    ((>)        (list 107))
    ((>=)       (list 108))
    ((+)        (list 109))
    ((-)        (list 110))
    ((*)        (list 111))
    ((/)        (list 112))
    (else (static-wrong "Cannot integrate" address))))

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

(define (ARITY=? arity+1)
  (case arity+1
    ((1 2 3 4) (list (+ 70 arity+1)))
    (else      (list 75 arity+1))))

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

(define (ARITY>=? arity+1) (list 78 arity+1))

(define-instruction (ARITY>=? arity+1) 78
  (if (not (>= (activation-frame-argument-length *val*) arity+1))
      (signal-exception #f (list "Too few function arguments"))))

(define (EXTEND-ENV) (list 32))
(define-instruction (EXTEND-ENV) 32
  (set! *env* (sr-extend* *env* *val*)))

(define (UNLINK-ENV) (list 33))
(define-instruction (UNLINK-ENV) 33
  (set! *env* (activation-frame-next *env*)))

(define (PUSH-VALUE) (list 34))
(define-instruction (PUSH-VALUE) 34
  (stack-push *val*))

(define (POP-ARG1) (list 35))
(define-instruction (POP-ARG1) 35
  (set! *arg1* (stack-pop)))

(define (POP-ARG2) (list 36))
(define-instruction (POP-ARG2) 36
  (set! *arg2* (stack-pop)))

(define (PRESERVE-ENV) (list 37))
(define-instruction (PRESERVE-ENV) 37
  (preserve-environment))

(define (RESTORE-ENV) (list 38))
(define-instruction (RESTORE-ENV) 38
  (restore-environment))

(define (POP-FUNCTION) (list 39))
(define-instruction (POP-FUNCTION) 39
  (set! *fun* (stack-pop)))

(define (CREATE-CLOSURE offset)
  (list 40 offset))
(define-instruction (CREATE-CLOSURE offset) 40
  (set! *val* (make-closure (+ *pc* offset) *env*)))

(define (RETURN) (list 43))
(define-instruction (RETURN) 43
  (set! *pc* (stack-pop)))

(define (PACK-FRAME! arity) (list 44 arity))
(define-instruction (PACK-FRAME! arity) 44
  (listify! *val* arity))

(define (FUNCTION-INVOKE) (list 45))
(define-instruction (FUNCTION-INVOKE) 45
  (invoke *fun* #f))

(define (FUNCTION-GOTO) (list 46))
(define-instruction (FUNCTION-GOTO) 46
  (invoke *fun* #t))

(define (POP-CONS-FRAME! arity) (list 47 arity))
(define-instruction (POP-CONS-FRAME! arity) 47
  (set-activation-frame-argument!
    *val* arity (cons (stack-pop)
                      (activation-frame-argument *val* arity))))

(define-instruction (DYNAMIC-REF index) 240
  (set! *val* (find-dynamic-value index)))

(define-instruction (DYNAMIC-POP) 241
  (pop-dynamic-binding))

(define-instruction (DYNAMIC-PUSH index) 242
  (push-dynamic-binding index *val*))

(define-instruction (NON-CONT-ERR) 245
  (signal-exception #f (list "Attempt to continue non-continuable exception")))

(define-instruction (PUSH-HANDLER) 246
  (push-exception-handler))

(define-instruction (POP-HANDLER) 247
  (pop-exception-handler))

(define-instruction (POP-ESCAPER) 250
  (let* ((tag (stack-pop))
         (escape (stack-pop)))
    (restore-environment)))

(define-instruction (PUSH-ESCAPER offset) 251
  (preserve-environment)
  (let* ((escape (make-escape (+ *stack-index* 3)))
         (frame (allocate-activation-frame 1)))
    (set-activation-frame-argument! frame 0 escape)
    (set! *env* (sr-extend* *env* frame))
    (stack-push escape)
    (stack-push escape-tag)
    (stack-push (+ *pc* offset))))

(define (FINISH) (list 20))
(define-instruction (FINISH) 20
  ;(*exit* *val*))
  (set! *exit* #t))

; ===========================================================================

(define (quotation-fetch i)
  (vector-ref *constants* i))

(define (preserve-environment)
  (stack-push *env*)
  (stack-push *dynenv*))

(define (restore-environment)
  (set! *dynenv* (stack-pop))
  (set! *env* (stack-pop)))

; ===========================================================================

(define (stack-push v)
  (vector-set! *stack* *stack-index* v)
  (set! *stack-index* (+ *stack-index* 1)))

(define (stack-pop)
  (set! *stack-index* (- *stack-index* 1))
  (vector-ref *stack* *stack-index*))

(define (make-closure code closed-environment)
  (list 'closure code closed-environment))

(define (closure? obj)
  (and (pair? obj)
       (eq? (car obj) 'closure)))

(define (closure-code obj)
  (cadr obj))

(define (closure-closed-environment obj)
  (caddr obj))

(define (make-primitive obj) obj)

(define primitive? procedure?)

(define (primitive-address obj) obj)

; ===========================================================================

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

; ===========================================================================

(define (save-stack)
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy))

(define (restore-stack copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*))

(define original-vector-copy! vector-copy!)
(define (vector-copy! old new start end)
  (original-vector-copy! new start old start (- end start)))


(define (make-continuation stack)
  `(continuation ,stack))

(define (continuation? obj)
  (and (pair? obj) (eq? (car obj) 'continuation)))

(define (continuation-stack obj)
  (cadr obj))

(define (continuation-invoke f tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (begin
        (restore-stack (continuation-stack f))
        (set! *val* (activation-frame-argument *val* 0))
        (set! *pc* (stack-pop)))
      (signal-exception #f (list "Incorrect arity" 'continuation))))


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


; ===========================================================================

(define (chapter7d-interpreter)
  (define (toplevel)
    (display ((stand-alone-producer7d (read)) 100))
    (toplevel))
  (toplevel))

(define (stand-alone-producer7d e)
  (set! g.current (original.g.current))
  (set! *quotations* '())
  (let* ((code (make-code-segment (meaning e r.init #t)))
         (start-pc (length (code-prologue)))
         (global-names (map car (reverse g.current)))
         (constants (apply vector *quotations*))
         (dynamics '()))
    (lambda (stack-size)
      (run-machine stack-size start-pc code
                   constants global-names dynamics))))

(define (make-code-segment m)
  (apply vector (append (code-prologue) m (RETURN))))

(define (code-prologue)
  (set! finish-pc 1)
  (append (NON-CONT-ERR) (FINISH) (POP-HANDLER) (RESTORE-ENV) (RETURN)))

; I want to preserve modified globals in the REPL
(let ((global-names (map car (reverse g.current))))
  (set! sg.current (make-vector (length global-names) undefined-value))
  (set! sg.current.names global-names))

(define (run-machine stack-size pc code constants global-names dynamics)
  (define base-error-handler-primitive
    (make-primitive base-error-handler))
  ;(set! sg.current (make-vector (length global-names) undefined-value))
  ;(set! sg.current.names global-names)
  (set! *constants* constants)
  (set! *dynamic-variables* dynamics)
  (set! *code* code)
  (set! *env* sr.init)
  (set! *dynenv* '())
  (set! *stack* (make-vector stack-size))
  (set! *stack-index* 0)
  (set! *val* 'anything)
  (set! *fun* 'anything)
  (set! *arg1* 'anything)
  (set! *arg2* 'anything)
  (push-dynamic-binding 0 (list base-error-handler-primitive))
  (stack-push finish-pc)
  (set! *pc* pc)
  (set! *exit* #f)
  ;(call/cc (lambda (exit)
  ;           (set! *exit* exit)
  ;           (run)))
  (run))

(define (base-error-handler)
  ;(show-registers "Panic error: content of registers")
  ;(wrong "Abort"))
  (wrong "Unhandled exception:" (activation-frame-argument *val* 1)))

(chapter7d-interpreter)
