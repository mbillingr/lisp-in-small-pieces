(import (builtin core)
        (libs utils)
        (libs book))

(include "common-stuff.scm")
(include "instructions.scm")

(define *constants* '())
(define *code* '())

(define (build-application application-name ofilename . ofilenames)
  (set! sg.current.names    '())
  (set! *dynamic-variables* '())
  (set! sg.current          (vector))
  (set! *constants*         (vector))
  (set! *code*              (vector))
  (define (install filenames entry-points)
    (if (pair? filenames)
        (let ((ep (install-object-file! (car filenames))))
          (install (cdr filenames) (cons ep entry-points)))
        (write-result-file application-name
                           (cons ";;; Bytecode application containing "
                                 (cons ofilename ofilenames))
                           *dynamic-variables*
                           sg.current.names
                           *constants*
                           *code*
                           entry-points)))
  (install (cons ofilename ofilenames) '()))

(define (install-object-file! filename)
  (let* ((ofilename (string-append filename ".sco"))
         (content (file-read ofilename))
         (dynamics (car content))
         (global-names (cadr content))
         (constants (caddr content))
         (code (cadddr content))
         (entry (car (cddddr content))))
    (relocate-globals! code global-names)
    (relocate-constants! code constants)
    (relocate-dynamics! code dynamics)
    (+ entry (install-code! code))))

(define (relocate-constants! code constants)
  (define n (vector-length *constants*))
  (let ((code-size (vector-length code)))
    (define (scan pc)
      (if (< pc code-size)
          (let ((instr (vector-ref code pc)))
            (if (= instr CONSTANT-code)
                (let* ((i (vector-ref code (+ pc 1)))
                       (quotation (vector-ref constants i)))
                  (vector-set! code (+ pc 1) (+ n i))))
            (scan (+ pc (instruction-size code pc))))))
    (scan 0)
    (set! *constants* (vector-append *constants* constants))))

(define (relocate-globals! code global-names)
  (define (get-index name)
    (let ((where (memq name sg.current.names)))
      (if where
          (- (length where) 1)
          (begin (set! sg.current.names (cons name sg.current.names))
                 (get-index name)))))
  (let ((code-size (vector-length code)))
    (define (scan pc)
      (if (< pc code-size)
          (let ((instr (vector-ref code pc)))
            (if (or (= instr CHECKED-GLOBAL-REF-code)
                    (= instr GLOBAL-REF-code)
                    (= instr SET-GLOBAL!-code))
                (let* ((i (vector-ref code (+ pc 1)))
                       (name (list-ref global-names i)))
                  (vector-set! code (+ pc 1) (get-index name))))
            (scan (+ pc (instruction-size code pc))))))
    (scan 0))
  (let ((v (make-vector (length sg.current.names))))
    (vector-copy! v 0 sg.current 0 (vector-length sg.current))
    (set! sg.current v)))

(define (relocate-dynamics! code dynamics)
  (for-each get-dynamic-variable-index dynamics)
  (let ((dynamics (reverse dynamics))
        (code-size (vector-length code)))
    (define (scan pc)
      (if (< pc code-size)
          (let ((instr (vector-ref code pc)))
            (if (or (= instr DYNAMIC-REF-code)
                    (= instr DYNAMIC-PUSH-code))
                (let* ((i (vector-ref code (+ pc 1)))
                       (name (list-ref dynamics (- i 1))))
                  (vector-set! code (+ pc 1)
                               (get-dynamic-variable-index name))))
            (scan (+ pc (instruction-size code pc))))))
    (scan 0)))

; ===========================================================================

(build-application "07-compilation/tests" "07-compilation/test1" "07-compilation/test2")