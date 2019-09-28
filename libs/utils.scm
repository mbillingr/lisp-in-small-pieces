
(define-library (libs utils)

  (export <= >=
          abs accumulate append assoc assq assv average
          caar cadr cdar cddr caaar caadr caddr cadar cdadr cddar cdddr caaddr
          cadddr cddddr
          cons-stream cube
          debug-eval debug-print dec delay
          enumerate even?
          false fixed-point force for-each
          gcd get get-coercion
          inc iterative-improve
          length list-tail
          map map1 memo-proc memq modulo
          nil
          power println printn put put-coercion
          reverse
          stream-car stream-cdr stream-null? sqr sqrt symbol<?
          tagged-list? the-empty-stream timeit trace true
          untrace
          vector
          xor)

  (import (builtin core)
          (libs table))

  (begin
    (define nil (list))
    (define false #f)
    (define true #t)

    (define-syntax debug-eval
      (syntax-rules ()
        ((_ expr) (debug (quote expr)))))

    (define (timeit f)
      (define (measure f start)
        (f)
        (- (runtime) start))
      (define (report n sum sqsum)
        (display n)
        (display "x -- ")
        (display (/ sum n))
        (display " +- ")
        (display (sqrt (/ (- sqsum (/ (sqr sum) n)) (- n 1))))
        (newline))
      (define (iter n sum sqsum end)
        (if (and (> n 3) (> (runtime) end))
            (report n sum sqsum)
            (let ((time (measure f (runtime))))
              (iter (+ n 1)
                    (+ sum time)
                    (+ sqsum (sqr time))
                    end))))
      (iter 0 0 0 (+ (runtime) 1e6)))

    (define (make-traced proc name)
      (let ((level 0))
        (lambda args
          (if (equal? args '(untrace))
              proc
              (begin (set! level (+ level 1))
                     (printn "> " level)
                     (println (cons name args))
                     (let ((result (apply proc args)))
                       (printn "< " level)
                       (println result)
                       (set! level (- level 1))
                       result))))))

    (define-syntax trace
      (syntax-rules ()
        ((_ name)
         (define name (make-traced name 'name)))))

    (define-syntax untrace
      (syntax-rules ()
        ((_ name)
         (define name (name 'untrace)))))

    (define (caar p) (car (car p)))
    (define (cadr p) (car (cdr p)))
    (define (cdar p) (cdr (car p)))
    (define (cddr p) (cdr (cdr p)))
    (define (caaar p) (car (car (car p))))
    (define (caadr p) (car (car (cdr p))))
    (define (cadar p) (car (cdr (car p))))
    (define (caddr p) (car (cdr (cdr p))))
    (define (cdadr p) (cdr (car (cdr p))))
    (define (cddar p) (cdr (cdr (car p))))
    (define (cdddr p) (cdr (cdr (cdr p))))
    (define (caaddr p) (car (car (cdr (cdr p)))))
    (define (cadddr p) (car (cdr (cdr (cdr p)))))
    (define (cddddr p) (cdr (cdr (cdr (cdr p)))))

    (define (abs x) (if (< x 0) (- x) x))
    (define (inc n) (+ n 1))
    (define (dec n) (- n 1))

    (define (even? x) (= 0 (modulo x 2)))
    (define (>= a b) (or (< b a) (= a b)))
    (define (<= a b) (or (< a b) (= a b)))

    (define (modulo a b)
     (define (fix a b r)
      (cond ((and (< a 0) (> b 0)) (+ b r))
            ((and (> a 0) (< b 0)) (+ b r))
            (else r)))
     (fix a b (remainder a b)))

    (define (symbol<? a b)
      (< a b))

    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))


    (define (average a b) (/ (+ a b) 2))
    (define (sqr x) (* x x))
    (define (cube x) (* x x x))
    (define (sqrt x)
      (define (improve guess)
        (average guess (/ x guess)))
      (fixed-point improve 1.0))

    (define (power base exponent)
      (if (< exponent 0)
          (error "exponent less than zero -- EXP" base exponent))
      (define (exp-iter a b n)
        (cond ((= n 0) a)
              ((even? n) (exp-iter a
                                   (* b b)
                                   (/ n 2)))
              (else (exp-iter (* a b)
                              b
                              (- n 1)))))
      (exp-iter 1 base exponent))


    (define (xor a b)
      (and (or a b)
           (not (and a b))))


    (define tolerance 1e-12)

    (define (iterative-improve good-enough? improve)
      (define (iter guess)
        (let ((next (improve guess)))
             (if (good-enough? guess next)
                 next
                 (iter next))))
      iter)

    (define (fixed-point f first-guess)
      (define (good-enough? guess next)
        (< (abs (- guess next)) tolerance))
      ((iterative-improve good-enough? f) first-guess))

    (define (reverse seq)
      (define (iter in out)
        (if (null? in)
            out
            (iter (cdr in)
                  (cons (car in) out))))
      (iter seq '()))

    (define (list-tail seq n)
      (if (= n 0)
          seq
          (list-tail (cdr seq) (- n 1))))

    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (assv key records)
      (cond ((null? records) false)
            ((eqv? key (caar records)) (car records))
            (else (assv key (cdr records)))))

    (define (assq key records)
      (cond ((null? records) false)
            ((eq? key (caar records)) (car records))
            (else (assq key (cdr records)))))

    (define (accumulate op initial sequence)
      (if (null? sequence)
          initial
          (op (car sequence)
              (accumulate op initial (cdr sequence)))))

    (define (map1 p sequence)
      (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

    (define (append . seqs)
      (accumulate append-2 '() seqs))

    (define (append-2 seq1 seq2)
      (accumulate cons seq2 seq1))

    (define (length sequence)
      (accumulate (lambda (x y) (+ y 1)) 0 sequence))

    (define (tagged-list? exp tag)
      (if (pair? exp)
          (eq? (car exp) tag)
          false))

    (define (for-each proc seq)
      (cond ((null? seq)
             'done)
            (else
              (proc (car seq))
              (for-each proc (cdr seq)))))

    (define (map op . seqs)
      (if (null? (car seqs))
          nil
          (cons (apply op (map1 car seqs))
                (apply map op (map1 cdr seqs)))))

    (define (enumerate seq)
      (define (scan s n)
        (if (null? s)
            '()
            (cons (list n (car s))
                  (scan (cdr s) (+ n 1)))))
      (scan seq 0))

    (define (memq item x)
      (cond ((null? x) false)
            ((eq? item (car x)) x)
            (else (memq item (cdr x)))))

    (define (println . args)
      (if (null? args)
          (newline)
          (begin (display (car args))
                 (display " ")
                 (apply println (cdr args)))))

    (define (printn x n)
      (cond ((> n 0) (display x)
                     (printn x (- n 1)))))


    (define (debug-print x)
      (println "DEBUG: " x)
      x)

    (define (memo-proc proc)
      (let ((already-run? false) (result false))
        (lambda ()
          (if already-run?
              result
              (begin (set! result (proc))
                     (set! already-run? true)
                     result)))))

    (define-syntax delay
      (syntax-rules ()
        ((_ x) (memo-proc (lambda () x)))))

    (define (force delayed)
      (delayed))

    (define-syntax cons-stream
      (syntax-rules ()
        ((_ a b) (cons a (delay b)))))

    (define the-empty-stream '())
    (define (stream-null? stream) (null? stream))
    (define (stream-car stream) (car stream))
    (define (stream-cdr stream) (force (cdr stream)))

    (define (vector . args)
      (let ((vec (make-vector (length args))))
        (define (init idx rest-args)
          (if (pair? rest-args)
              (begin (vector-set! vec idx (car rest-args))
                     (init (+ idx 1) (cdr rest-args)))
              vec))
        (init 0 args)))

    ;; ==========================================
    ;;   put and get into a global table
    ;;   (need those in exercise 2.73 and maybe later)
    ;; ==========================================

    (define (impl-table)
      (define table (make-table))

      (define (make-key op type)
        (cons op type))

      (define (put op type item)
        (set! table
              (insert (make-key op type)
                      item
                      table)))

      (define (get op type)
        (let ((record (lookup (make-key op type)
                              table)))
          (and record (record))))

      ; export public functions
      (list put get))

    (define put '())
    (define get '())
    (let ((put_get (impl-table)))
      (set! put (car put_get))
      (set! get (cadr put_get)))

    (define put-coercion '())
    (define get-coercion '())
    (let ((put_get (impl-table)))
      (set! put-coercion (car put_get))
      (set! get-coercion (cadr put_get)))))
