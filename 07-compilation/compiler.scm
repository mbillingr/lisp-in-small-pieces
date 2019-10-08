(import (builtin core)
        (libs utils)
        (libs book))

(include "common-stuff.scm")
(include "pretreatment2.scm")
(include "primitives.scm")

(define (read-file filename)
  (file-read filename))


(define *quotations* '())

(define (compile-file filename)
  (set! g.current '())
  (set! *quotations* '())
  (set! *dynamic-variables* '())
  (let* ((complete-filename (string-append filename ".scm"))
         (e                 `(begin . ,(read-file complete-filename)))
         (code              (make-code-segment (meaning e r.init #t)))
         (global-names      (map car (reverse g.current)))
         (constants         (apply vector *quotations*))
         (dynamics          *dynamic-variables*)
         (ofilename         (string-append filename ".sco")))
    (write-result-file ofilename
                       (list ";;; Bytecode object file for " complete-filename)
                       dynamics global-names constants code
                       (length (code-prologue)))))

; ===========================================================================

(define (compute-kind r n)
  (or (local-variable? r 0 n)
      (global-variable? g.current n)
      (global-variable? g.init n)
      (adjoin-global-variable! n)))

(define (adjoin-global-variable! name)
  (let ((index (g.current-extend! name)))
    (cdr (car g.current))))

(define (make-code-segment m)
  (apply vector (append (code-prologue) m (RETURN))))

(define (code-prologue)
  ;(set! finish-pc 1)
  (append (NON-CONT-ERR) (FINISH) (POP-HANDLER) (RESTORE-ENV) (RETURN)))

(define (check-byte j)
  (if (or (< j 0) (> j 255))
      (static-wrong "Cannot pack this number within a byte" j)))

; ===========================================================================
;  Commbinators

(define (ESCAPER m+)
  (append (PUSH-ESCAPER (+ 1 (length m+)))
          m+
          (RETURN)
          (POP-ESCAPER)))

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
;  Instructions

(define (SHALLOW-ARGUMENT-REF j)
  (check-byte j)
  (case j
    ((0 1 2 3) (list (+ 1 j)))
    (else      (list 5 j))))

(define (DEEP-ARGUMENT-REF i j) (list 6 i j))
(define (GLOBAL-REF i)          (list 7 i))
(define (CHECKED-GLOBAL-REF i)  (list 8 i))
(define (EXPLICIT-CONSTANT value)
  (set! *quotations* (append *quotations* (list value)))
  (list 9 (- (length *quotations*) 1)))

(define (PREDEFINED i)
  (check-byte i)
  (case i
    ((0 1 2 3 4 5 6 7 8) (list (+ 10 i)))
    (else                (list 19 i))))

(define (FINISH) (list 20))

(define (SET-SHALLOW-ARGUMENT! j)
  (case j
    ((0 1 2 3) (list (+ 21 j)))
    (else      (list 25 j))))

(define (SET-DEEP-ARGUMENT! i j) (list 26 i j))
(define (SET-GLOBAL! i)          (list 27 i))

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

(define (EXTEND-ENV)            (list 32))
(define (UNLINK-ENV)            (list 33))
(define (PUSH-VALUE)            (list 34))
(define (POP-ARG1)              (list 35))
(define (POP-ARG2)              (list 36))
(define (PRESERVE-ENV)          (list 37))
(define (RESTORE-ENV)           (list 38))
(define (POP-FUNCTION)          (list 39))
(define (CREATE-CLOSURE offset) (list 40 offset))

(define (RETURN)                (list 43))
(define (PACK-FRAME! arity)     (list 44 arity))
(define (FUNCTION-INVOKE)       (list 45))
(define (FUNCTION-GOTO)         (list 46))
(define (POP-CONS-FRAME! arity) (list 47 arity))

(define (ALLOCATE-FRAME size)
  (case size
    ((0 1 2 3 4) (list (+ 50 size)))
    (else        (list 55 (+ size 1)))))

(define (ALLOCATE-DOTTED-FRAME arity) (list 56 (+ arity 1)))

(define (POP-FRAME! rank)
  (case rank
    ((0 1 2 3)  (list (+ 60 rank)))
    (else       (list 64 rank))))

(define (ARITY=? arity+1)
  (case arity+1
    ((1 2 3 4) (list (+ 70 arity+1)))
    (else      (list 75 arity+1))))

(define (ARITY>=? arity+1) (list 78 arity+1))

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

(define (DYNAMIC-REF index)   (list 240 index))
(define (DYNAMIC-POP)         (list 241))
(define (DYNAMIC-PUSH index)  (list 242 index))

(define (NON-CONT-ERR)        (list 245))
(define (PUSH-HANDLER)        (list 246))
(define (POP-HANDLER)         (list 247))

(define (POP-ESCAPER)         (list 250))
(define (PUSH-ESCAPER offset) (list 251 offset))

; ===========================================================================

(compile-file "07-compilation/test1")
(compile-file "07-compilation/test2")
(compile-file "07-compilation/test3")
(compile-file "07-compilation/test4")
