(import (builtin core)
        (libs utils)
        (libs book))

(define (lcons a d) (lambda (selector) (selector a d)))
(define (lcar pair) (pair (lambda (a d) a)))
(define (lcdr pair) (pair (lambda (a d) d)))
