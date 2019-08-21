(import (builtin core)
        (libs utils))

(define fix
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda (x) (((w w) f) x)))))))
    (d d)))

(define (meta-fact f)
  (lambda (n)
    (if (= n 0)
        1
        (* n (f (- n 1))))))


(define fix2
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda (x y) (((w w) f) x y)))))))
    (d d)))

(define (meta-fact-n f)
  (lambda (n m)
    (if (= n 0)
        m
        (* n (f (- n 1) m)))))

(println ((fix2 meta-fact-n) 5 2))

(define fixN
  (let ((d (lambda (w)
             (lambda (f)
               (f (lambda args (apply ((w w) f) args)))))))
    (d d)))

(define (meta-fact-n f)
  (lambda (n m)
    (if (= n 0)
        m
        (* n (f (- n 1) m)))))

(println ((fixN meta-fact-n) 5 2))

(define (meta-fact-n f)
  (lambda (n m s)
    (if (= n s)
        m
        (* n (f (- n 1) m s)))))

(println ((fixN meta-fact-n) 5 1 3))
