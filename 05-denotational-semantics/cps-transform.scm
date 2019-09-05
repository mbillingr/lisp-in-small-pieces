(import (builtin core)
        (libs utils)
        (libs book))

(define (cps e)
  (if (atom? e)
      (lambda (k) (k `,e))
      (case (car e)
        ((quote)  (cps-quote (cadr e)))
        ((if)     (cps-if (cadr e) (caddr e) (cadddr e)))
        ((begin)  (cps-begin (cdr e)))
        ((set!)   (cps-set! (cadr e) (caddr e)))
        ((lambda) (cps-abstraction (cadr e) (caddr e)))
        (else     (cps-application e)))))

(define (cps-quote data)
  (lambda (k)
    (k `(quote ,data))))

(define (cps-set! variable form)
  (lambda (k)
    ((cps form)
     (lambda (a)
       (k `(set! ,variable ,a))))))

(define (cps-if cond form1 form2)
  (lambda (k)
    ((cps cond)
     (lambda (c)
       `(if ,c ,((cps form1) k)
               ,((cps form2) k))))))

(define (cps-begin e)
  (if (pair? e)
      (if (pair? (cdr e))
          (let ((void (gensym "void")))
            (lambda (k)
              ((cps (car e))
               (lambda (b)
                 ((cps-begin (cdr e))
                  (lambda (a)
                    (k `((lambda (,void) ,b) ,a))))))))
          (cps (car e)))
      (cps '())))

(define (cps-application e)
  (lambda (k)
    (if (memq (car e) primitives)
        ((cps-terms (cdr e))
         (lambda (t*)
           ;(k `(,(car e) ,@t*))))
           (k (cons (car e) t*))))
        ((cps-terms e)
         (lambda (t*)
           (let ((d (gensym)))
             `(,(car t*) (lambda (,d) ,(k d))
                         . ,(cdr t*))))))))

(define primitives '(cons car cdr list * + - = pair? eq?))

(define (cps-terms e*)
  (if (pair? e*)
      (lambda (k)
        ((cps (car e*))
         (lambda (a)
           ((cps-terms (cdr e*))
            (lambda (a*)
              (k (cons a a*)))))))
      (lambda (k) (k '()))))

(define (cps-abstraction variables body)
  (lambda (k)
    (k (let ((c (gensym "cont")))
         `(lambda (,c . ,variables)
            ,((cps body)
              (lambda (a) `(,c ,a))))))))


(define (call/cc k f) (f k k))


(define fact-program
  '(set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
(define fact-cps ((cps fact-program) (lambda (x) x)))

(define source-foo '(lambda (exit) (begin (exit 42) 666)))
(define target-foo ((cps source-foo) (lambda (x) x)))

; The function foo is transformed to something like this:
'(set! foo
   (lambda (cont2 exit)
     (exit (lambda (sym4)
             (cont2 ((lambda (void3) 666)
                     sym4)))
           42)))


(define source-bar `(lambda () (call/cc ,source-foo)))
(define target-bar ((cps source-bar) (lambda (x) x)))

; bar becomes
'(lambda (cont5)
  (call/cc (lambda (sym9) (cont5 sym9))
           (lambda (cont6 exit)
             (exit (lambda (sym8) (cont6 ((lambda (void7) sym8) 666)))
                   42))))

; which is equivalent to
'(lambda (cont5)
  ((lambda (cont6 exit)
    (exit (lambda (sym8) (cont6 ((lambda (void7) sym8) 666)))
          42))
   (lambda (sym9) (cont5 sym9))
   (lambda (sym9) (cont5 sym9))))

; and the same as this:
'(lambda (cont5)
    ((lambda (sym9) (cont5 sym9))
     (lambda (sym8) ((lambda (sym9) (cont5 sym9)) ((lambda (void7) sym8) 666)))
     42))

; here is a problem: (lambda (sym9) ... ) is invoked with two arguments.


; Apparent solution (inspired by http://matt.might.net/articles/cps-conversion/):
(define (call/cc k f) (f k (lambda (_ x) (k x))))

; This definition of call/cc wraps the continuation passed to f in a lambda
; form that takes two arguments but ignores the continuation passed as first
; argument.
; I wonder if this is an oversight in the book, or if I misunderstood something...
