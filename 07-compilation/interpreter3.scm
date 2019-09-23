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

(define-instruction (SHALLOW-ARGUMENT-REF j) 5
  (set! *val* (activation-frame-argument *env* j)))

(define-instruction (DUMMY) 0
  (println "DUMMY"))

(define-instruction (INT x) 1
  (println x))

(define *code* (make-vector 3))
(vector-set! *code* 0 1)
(vector-set! *code* 1 42)
(vector-set! *code* 2 0)
(define *pc* 0)

; ===========================================================================
