(import (builtin core)
        (libs utils)
        (libs book))

; definition of (eq?) to compare dotted pairs with the help of (set-car!) or (set-cdr!)

(define (eq? a b)
  (if (and (pair? a) (pair? b))
      (let ((original_a (car a))
            (original_b (car b)))
        (set-car! b 0)
        (set-car! a 1)
        (let ((same (= (car b) (car a))))  ; pairs are the same if the modification of one is visible in the other
          (set-car! b original_b)
          (set-car! a original_a)
          same))
      (equal? a b)))  ; delegate the uninteresting part to an existing function

(define x (cons 'A 'B))
(define y (cons 'A 'B))

(println (eq? x x))
(println (eq? y y))
(println (eq? x y))

(println x y)
