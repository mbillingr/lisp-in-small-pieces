(import (builtin core)
        (libs utils)
        (libs book)
        (06-fast-interpreter common))

; instructions are encoded as bytecode

(include "pretreatment.scm")


; ===========================================================================
(define (run)
  (let ((instruction (fetch-byte)))
    ((vector-ref instruction-body-table instruction)))
  (run))

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
(define-instruction (PREDEFINED3) 13 (set! *val* cons))
(define-instruction (PREDEFINED4) 14 (set! *val* car))
(define-instruction (PREDEFINED5) 15 (set! *val* cdr))
(define-instruction (PREDEFINED6) 16 (set! *val* pair?))
(define-instruction (PREDEFINED7) 17 (set! *val* symbol?))
(define-instruction (PREDEFINED8) 18 (set! *val* eq?))
(define-instruction (PREDEFINED i) 19
  (set! *val* (predefined-fetch i)))



; ===========================================================================

(define (quotation-fetch i)
  (vector-ref *constants* i))
