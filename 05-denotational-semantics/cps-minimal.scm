(import (builtin core)
        (libs utils)
        (libs book))

(define (cps e)
  (if (pair? e)
      (case (car e)
        ; ...
        (else (cps-application e)))
      (lambda (k) (k `,e))))

(define (cps-application e)
  (lambda (k)
    ((cps-terms e)
     (lambda (t*)
       (let ((d (gensym)))
         `(,(car t*) (lambda (,d) ,(k d))
                     . ,(cdr t*)))))))

(define (cps-terms e*)
  (if (pair? e*)
      (lambda (k)
        ((cps (car e*))
         (lambda (a)
           ((cps-terms (cdr e*))
            (lambda (a*)
              (k (cons a a*)))))))
      (lambda (k) (k '()))))


(define (call/cc k f) (f k k))

; transformation examples...

;((cps '(call/cc (lambda (return) (begin (return 2) 3))))
; (lambda (x) x))
;
; results in:
;
;(call/cc (lambda (sym19) sym19)
;         (lambda (cont16 return)
;           (return (lambda (sym18) (cont16 ((lambda (void17) sym18) 3)))
;                   2)

;((cps '(lambda (return) (begin (return 2) 3))) (lambda (x) x))
;
; results in:
;
;(lambda (cont9 return)
;  (return (lambda (sym11)
;            (cont9 ((lambda (void10) sym11) 3))
;          2)


(define fact-program
  '(set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))))
(define fact-cps ((cps fact-program) (lambda (x) x)))

(define foo '())
(define test-cc '(set! foo (lambda (exit) (begin (exit 42) 666))))
(define test-cps ((cps test-cc) (lambda (x) x)))

; The function foo is transformed to something like this:
'(set! foo
   (lambda (cont2 exit)
     (exit (lambda (sym4)
             (cont2 ((lambda (void3) 666)
                     sym4)))
           42)))
; The goal is to call it with (call/cc foo) to return 42.
; (call/cc foo) becomes (call/cc (lambda (sym5) sym5) foo).
;
; There is a problem: continuations expect one argument but
; (exit) is called with two arguments in foo.
; TODO: how can this be fixed?


(define foo
  '(lambda (exit)
     ((lambda (x) 666) (exit 42))))

(define (M e)
  (if (pair? e)
      (case (car e)
        ((lambda) (let ((k (gensym "k")))
                    `(lambda (,k . ,(cadr e)) ,(T (caddr e) k))))
        (else e))
      e))

(define (T expr cont)
  (if (pair? expr)
      (case (car expr)
        ((lambda) `(,cont ,(M expr)))
        (else (let ((f (gensym "f"))
                    (e (gensym "e")))
                (T (car expr) `(lambda (,f)
                                      ,(T (cadr expr) `(lambda (,e)
                                                         (,f ,e ,cont))))))))
      `(,cont ,(M expr))))


(lambda (k992 exit)
  ((lambda (f993)
     ((lambda (f995)
        ((lambda (e996)
           (f995 e996 (lambda (e994) (f993 e994 k992))))
         42))
      exit))
   (lambda (k997 x) (k997 666))))

(lambda (k992 exit)
  (exit 42 (lambda (e994) ((lambda (k997 x) (k997 666)) e994 k992))))
