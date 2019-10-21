(import (builtin core)
        (libs utils)
        (libs book))

(include "common-stuff.scm")
(include "instructions.scm")
(include "primitives.scm")

(define (disassemble-application filename)
  (let* ((content (file-read filename))
         (dynamics      (car content))
         (global-names  (cadr content))
         (constants     (caddr content))
         (code          (cadddr content))
         (entry-points  (car (cddddr content))))
    (define (loop pc)
      (if (< pc (vector-length code))
          (begin
            (println pc (instruction-decode code pc))
            (loop (+ pc (instruction-size code pc))))))
    (loop 0)))

; ===========================================================================

(disassemble-application
  "08-reflection/reflective-interpreter.sco")
