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
              ((cps-begin (cdr e))
               (lambda (b)
                 ((cps (car e))
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
