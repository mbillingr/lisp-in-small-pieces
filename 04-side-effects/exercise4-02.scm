(import (builtin core)
        (libs utils)
        (libs book))

; cons/car/cdr with only lambda terms

(define (lcons a d) (lambda (selector) (selector a d)))
(define (lcar pair) (pair (lambda (a d) a)))
(define (lcdr pair) (pair (lambda (a d) d)))
