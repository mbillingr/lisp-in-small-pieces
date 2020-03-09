 (define-library (kanren)
     (export == alwayso append-inf append-map-inf conda conde condu conj conj2
             defrel disj disj2 empty-s ext-s fail fresh ifte nevero occurs? once
             reify reify-name reify-s run run* run-goal run-tests succeed
             take-inf unify var var? walk)
     (import (scheme base)
             (sunny ports))  ; todo: replace with scheme standard library
     (begin
       ;  Topnotes
       ; ==========
       ; (1) A goal is a function that takes a substitution and, if it returns,
       ;     produces a stream of substitutions.
       ; (2) A substitution is a special kind of association list. It associates
       ;     variables with values or other variables. In a substitution, as
       ;     association whose cdr is also a variable represents the fusing of
       ;     that association's two variables.

       ; Create a unique variable.
       (define (var name) (vector name))

       (define (var? x) (vector? x))

       ; The empty substitution.
       (define empty-s '())

       ; Walk a substitution to look up a variable.
       (define (walk v s)
         (let ((a (and (var? v)
                       (assv v s))))
           (if (pair? a)
               (walk (cdr a) s)
               v)))

       ; Extend substitution s with as association between variable
       ; x and the value v, or produce #f if extending the
       ; substitution with the pair `(,x . ,v) would create a cycle.
       (define (ext-s x v s)
         (if (occurs? x v s)
             #f
             (cons `(,x . ,v) s)))

       ; Test if x occurs in v, given the substitution s.
       (define (occurs? x v s)
         (let ((v (walk v s)))
           (cond ((var? v) (eqv? v x))
                 ((pair? v) (or (occurs? x (car v) s)
                                (occurs? x (cdr v) s)))
                 (else #f))))

       ; Extend the substitution s by matching u and v, or
       ; return #f if this is not possible or a cycle would occur.
       (define (unify u v s)
         (let ((u (walk u s))
               (v (walk v s)))
           (cond ((eqv? u v) s)
                 ((var? u) (ext-s u v s))
                 ((var? v) (ext-s v u s))
                 ((and (pair? u) (pair? v))
                  (let ((s (unify (car u) (car v) s)))
                    (and s (unify (cdr u) (cdr v) s))))
                 (else #f))))

       ; Produce a goal (1) that succeeds if u and v are equivalent
       (define (== u v)
         (lambda (s)
           (let ((s (unify u v s)))
             (if s `(,s) '()))))

       ; Produce a goal (1) that always succeeds
       (define succeed
         (lambda (s)
           `(,s)))

       ; Produce a goal (1) that always fails
       (define fail
         (lambda (s)
           '()))

       ; Produce a goal (1) that succeeds if either g1 or g2 succeeds
       (define (disj2 g1 g2)
         (lambda (s)
           (append-inf (g1 s) (g2 s))))

       ; Append two streams. If the streams contain suspensions, they are
       ; spliced together alternatingly.
       (define (append-inf s-inf t-inf)
         (cond ((null? s-inf) t-inf)
               ((pair? s-inf)
                (cons (car s-inf)
                      (append-inf (cdr s-inf) t-inf)))
               (else (lambda () (append-inf t-inf (s-inf))))))

       ; A goal that never produces a substitution
       (define (nevero)
         (lambda (s)
           (lambda ()
             ((nevero) s))))

       ; A goal that always produces a substitution
       (define (alwayso)
         (lambda (s)
           (lambda ()
             ((disj2 succeed (alwayso)) s))))

       ; Produce a list of the first n substitutions. If n is #f get all
       ; substitutions, or never return if s-inf is an infinite stream.
       (define (take-inf n s-inf)
         (cond ((and n (zero? n)) '())
               ((null? s-inf) '())
               ((pair? s-inf) (cons (car s-inf)
                                    (take-inf (and n (- n 1))
                                              (cdr s-inf))))
               (else (take-inf n (s-inf)))))

       ; Produce a goal that succeeds if both, g1 and g2 succed.
       (define (conj2 g1 g2)
         (lambda (s)
           (append-map-inf g2 (g1 s))))

       ; Apply a function (goal) to each element of a stream and return a
       ; stream of results. In contrast to map, it uses append-inf instead of
       ; cons (as in map) to build the result. Can handle infinite streams with
       ; suspensions.
       (define (append-map-inf g s-inf)
         (cond ((null? s-inf) '())
               ((pair? s-inf)
                (append-inf (g (car s-inf))
                            (append-map-inf g (cdr s-inf))))
               (else (lambda () (append-map-inf g (s-inf))))))

       ; Create a fresh variable with name and pass it to the function f
       (define (call/fresh name f)
         (f (var name)))

       (define (reify-name n)
         (string->symbol
           (string-append "_" (number->string n))))

       ; Recursively walk all variables and insert their substitution if they
       ; are not fresh.
       (define (walk* v s)
         (let ((v (walk v s)))
           (cond ((var? v) v)
                 ((pair? v)
                  (cons (walk* (car v) s)
                        (walk* (cdr v) s)))
                 (else v))))

       ; Substitute all fresh variables in v with their reified names and
       ; adjoin to r.
       (define (reify-s v r)
         (let ((v (walk v r)))
           (cond ((var? v)
                  (let ((n (length r)))
                    (let ((rn (reify-name n)))
                      (cons `(, v . ,rn) r))))
                 ((pair? v)
                  (let ((r (reify-s (car v) r)))
                    (reify-s (cdr v) r)))
                 (else r))))

       ; Resolve all substitutions in v and reify the remaining fresh variables
       (define (reify v)
         (lambda (s)
           (let ((v (walk* v s)))
             (let ((r (reify-s v empty-s)))
               (walk* v r)))))

       ; Run the goal g, and return up to n resulting substitutions.
       (define (run-goal n g)
         (take-inf n (g empty-s)))

       ; Produce a goal that succeeds if g1 and g2 succeed or g1 fails
       ; and g3 succeeds.
       (define (ifte g1 g2 g3)
         (lambda (s)
           (let loop ((s-inf (g1 s)))
             (cond ((null? s-inf) (g3 s))
                   ((pair? s-inf)
                    (append-map-inf g2 s-inf))
                   (else (lambda ()
                           (loop (s-inf))))))))

       ; Produce a goal that succeeds at most once.
       (define (once g)
         (lambda (s)
           (let loop ((s-inf (g s)))
             (cond ((null? s-inf) '())
                   ((pair? s-inf)
                    (cons (car s-inf) '()))
                   (else (lambda ()
                           (loop (s-inf))))))))

       ; DSL Macros

       (define-syntax disj
         (syntax-rules ()
           ((disj) fail)
           ((disj g) g)
           ((disj g0 g ...) (disj2 g0 (disj g ...)))))

       (define-syntax conj
         (syntax-rules ()
           ((conj) succeed)
           ((conj g) g)
           ((conj g0 g ...) (conj2 g0 (conj g ...)))))

       (define-syntax defrel
         (syntax-rules ()
           ((defrel (name x ...) g ...)
            (define (name x ...)
              (lambda (s)
                (lambda ()
                  ((conj g ...) s)))))))

       (define-syntax run
         (syntax-rules ()
           ((run n (x0 x ...) g ...)
            (run n q (fresh (x0 x ...)
                       (== `(,x0 ,x ...) q) g ...)))
           ((run n q g ...)
            (let ((q (var 'q)))
              (map (reify q)
                (run-goal n (conj g ...)))))))

       (define-syntax run*
         (syntax-rules ()
           ((run* q g ...) (run #f q g ...))))

       (define-syntax fresh
         (syntax-rules ()
           ((fresh () g ...) (conj g ...))
           ((fresh (x0 x ...) g ...)
            (call/fresh 'x0
              (lambda (x0)
                (fresh (x ...) g ...))))))

       (define-syntax conde
         (syntax-rules ()
           ((conde (g ...) ...)
            (disj (conj g ...) ...))))

       (define-syntax conda
         (syntax-rules ()
           ((conda (g0 g ...)) (conj g0 g ...))
           ((conda (g0 g ...) ln ...)
            (ifte g0 (conj g ...) (conda ln ...)))))

       (define-syntax condu
         (syntax-rules ()
           ((condu (g0 g ...) ...)
            (conda ((once g0) g ...) ...))))

       ;;
       ;; Tests
       ;;

       (define (run-tests)
         (let-syntax
           ((assert (syntax-rules (that the value of is)
                      ((assert that the value of exp is val)
                       (begin
                         (display "the value of ")
                         (write 'exp)
                         (display " is the same as ")
                         (write 'val)
                         (let ((result exp))
                           (if (equal? result val)
                               (begin (display " : OK")
                                      (newline))
                               (begin (display " : FAIL ... got ")
                                      (write result)
                                      (display " instead.")
                                      (newline)
                                      (error "ASSERTION FAILED")))))))))

           ;; Testing the test suite

           (assert that the value of (+ 1 2) is 3)

           ;; Unit Tests (Chapter 10)

           (define u (var 'u)) (define v (var 'v)) (define w (var 'w))
           (define x (var 'x)) (define y (var 'y)) (define z (var 'z))

           (assert that the value of
             (walk z `((,z . a) (,x . ,w) (,y . ,z)))
             is 'a)
           (assert that the value of
             (walk y `((,z . a) (,x . ,w) (,y . ,z)))
             is 'a)
           (assert that the value of
             (walk x `((,z . a) (,x . ,w) (,y . ,z)))
             is w)

           (assert that the value of
             (walk x `((,x . ,y) (,v . ,x) (,w . ,x)))
             is y)
           (assert that the value of
             (walk v `((,x . ,y) (,v . ,x) (,w . ,x)))
             is y)
           (assert that the value of
             (walk w `((,x . ,y) (,v . ,x) (,w . ,x)))
             is y)

           (assert that the value of
             (walk w `((,x . b) (,z . ,y) (,w . (,x e ,z))))
             is `(,x e ,z))

           (assert that the value of
             (occurs? x x '())
             is #t)
           (assert that the value of
             (occurs? x `(,y) `((,y . ,x)))
             is #t)

           (assert that the value of
             (ext-s x `(,x) empty-s)
             is #f)
           (assert that the value of
             (ext-s x `(,y) `((,y . ,x)))
             is #f)

           (assert that the value of
             (let ((s `((,z . ,x) (,y . ,z))))
               (let ((s (ext-s x 'e s)))
                 (and s (walk y s))))
             is 'e)

           (assert that the value of
             ((== #t #f) empty-s)
             is '())
           (assert that the value of
             ((== #t #f) empty-s)
             is (fail empty-s))
           (assert that the value of
             ((== #f #f) empty-s)
             is (succeed empty-s))
           (assert that the value of
             ((== x y) empty-s)
             is `(((,x . ,y))))

           (assert that the value of
             ((disj2 (== 'olive x) (== 'oil x)) empty-s)
             is `(((,x . olive)) ((,x . oil))))

           (assert that the value of
             (let ((s-inf ((disj2
                             (== 'olive x)
                             (nevero))
                           empty-s)))
              (cons (car s-inf)
                    (procedure? (cdr s-inf))))
             is `(((,x . olive)) . #t))

           (assert that the value of
             (let ((s-inf ((disj2
                             (nevero)
                             (== 'olive x))
                           empty-s)))
              (let ((s2 (s-inf)))
                (cons (car s2)
                      (procedure? (cdr s2)))))
             is `(((,x . olive)) . #t))

           (assert that the value of
             (take-inf 3 ((alwayso) empty-s))
             is '(() () ()))

           (assert that the value of
             (take-inf #f ((disj2 (== 'olive x) (== 'oil x)) empty-s))
             is `(((,x . olive)) ((,x . oil))))


           (assert that the value of
             (take-inf #f ((conj2 (== 'olive x) (== 'oil x)) empty-s))
             is '())

           (assert that the value of
             (take-inf #f ((conj2 (== 42 x) succeed) empty-s))
             is `(((,x . 42))))

           (assert that the value of
             (walk* w `((,x . b) (,z . ,y) (,w . (,x e ,z))))
             is `(b e ,y))

           (assert that the value of
             (let ((a1 `(,x . (,u ,w ,y ,z ((ice) ,z))))
                   (a2 `(,y . corn))
                   (a3 `(,w . (,v ,u))))
               (let ((s `(,a1 ,a2 ,a3)))
                 ((reify x) s)))
             is '(_0 (_1 _0) corn _2 ((ice) _2)))

           (assert that the value of
             (map (reify x)
               (take-inf 5 ((disj2 (== 'olive x) (== 'oil x))
                            empty-s)))
             is '(olive oil))

           (assert that the value of
             (map (reify x)
               (run-goal 5 (disj2 (== 'olive x) (== 'oil x))))
             is '(olive oil))

           (assert that the value of
             ((ifte succeed
                    (== #f y)
                    (== #t y))
              empty-s)
             is `(((,y . #f))))

           (assert that the value of
             ((ifte fail
                    (== #f y)
                    (== #t y))
              empty-s)
             is `(((,y . #t))))

           (assert that the value of
             ((ifte (== #t x)
                    (== #f y)
                    (== #t y))
              empty-s)
             is `(((,y . #f) (,x . #t))))

           (assert that the value of
             ((ifte (disj2 (== #t x) (== #f x))
                    (== #f y)
                    (== #t y))
              empty-s)
             is `(((,y . #f) (,x . #t)) ((,y . #f) (,x . #f))))

           (assert that the value of
             ((ifte (once (disj2 (== #t x) (== #f x)))
                    (== #f y)
                    (== #t y))
              empty-s)
             is `(((,y . #f) (,x . #t))))

           ;; Integration Tests (Chapters 1-9)

           ; Chapter 1

           (assert that the value of
             (run* q fail)
             is '())

           (assert that the value of
             (run* q (== 'pea 'pod))
             is '())

           (assert that the value of
             (run* q (== q 'pea))
             is '(pea))

           (assert that the value of
             (run* q (== q 'pea))
             is (run* q (== 'pea q)))

           (assert that the value of
             (run* q succeed)
             is '(_0))

           (assert that the value of
             (run* q
               (fresh (x)
                 (== `(,x) q)))
             is '((_0)))

           (assert that the value of
             (run* q
               (fresh (x)
                 (== 'pea q)))
             is '(pea))

           (assert that the value of
             (run* q
               (fresh (x)
                 (== 'pea x)))
             is '(_0))

           (assert that the value of
             (run* q
               (fresh (x)
                 (== (cons x '()) q)))
             is '((_0)))

           (assert that the value of
             (run* q (== '(((pea)) pod) '(((pea)) pod)))
             is '(_0))

           (assert that the value of
             (run* q (== '(((pea)) pod) `(((pea)) ,q)))
             is '(pod))

           (assert that the value of
             (run* q (== `(((,q)) pod) '(((pea)) pod)))
             is '(pea))

           (assert that the value of
             (run* q (fresh (x)
                       (== `(((,q)) ,x) `(((,x)) pod))))
             is '(pod))

           (assert that the value of
             (run* q (fresh (x) (== `(,x ,x) q)))
             is '((_0 _0)))

           (assert that the value of
             (run* q (fresh (x) (fresh (y) (== `(,q ,y) `((,x ,y) ,x)))))
             is '((_0 _0)))

           (assert that the value of
             (run* q (fresh (x) (fresh (y)) (== `(,x ,y) q)))
             is '((_0 _1)))

           (assert that the value of
             (run* q (fresh (x) (fresh (y) (== `(,x ,y ,x) q))))
             is '((_0 _1 _0)))

           (assert that the value of
             (run* q (conj2 succeed succeed))
             is '(_0))

           (assert that the value of
             (run* q (conj2 succeed (== 'corn q)))
             is '(corn))

           (assert that the value of
             (run* q (conj2 fail (== 'corn q)))
             is '())

           (assert that the value of
             (run* q (conj2 (== 'corn q) (== 'meal q)))
             is '())

           (assert that the value of
             (run* q (conj2 (== 'corn q) (== 'corn q)))
             is '(corn))

           (assert that the value of
             (run* q (disj2 fail fail))
             is '())

           (assert that the value of
             (run* q (disj2 (== 'olive q) fail))
             is '(olive))

           (assert that the value of
             (run* q (disj2 fail (== 'oil q)))
             is '(oil))

           (assert that the value of
             (run* q (disj2 (== 'olive q) (== 'oil q)))
             is '(olive oil))

           (assert that the value of
             (run* q
               (fresh (x)
                 (fresh (y)
                   (disj2 (== `(,x ,y) q)
                          (== `(,y ,x) q)))))
             is '((_0 _1) (_0 _1)))

           (assert that the value of
             (run* x (disj2 (conj2 (== 'olive x) fail)
                            (== 'oil x)))
             is '(oil))

           (assert that the value of
             (run* x (disj2 (conj2 (== 'olive x) succeed)
                            (== 'oil x)))
             is '(olive oil))

           (assert that the value of
             (run* x (disj2 (== 'oil x)
                            (conj2 (== 'olive x) succeed)))
             is '(oil olive))

           (assert that the value of
             (run* x
               (disj2 (conj2 (== 'virgin x) fail)
                      (disj2 (== 'olive x)
                             (disj2 succeed
                                    (== 'oil x)))))
             is '(olive _0 oil))

           (assert that the value of
             (run* r
               (fresh (x)
                 (fresh (y)
                   (conj2 (== 'split x)
                          (conj2 (== 'pea y)
                                 (== `(,x ,y) r))))))
             is '((split pea)))

           (assert that the value of
             (run* r
               (fresh (x y)
                 (conj2 (== 'split x)
                        (conj2 (== 'pea y)
                               (== `(,x ,y) r)))))
             is '((split pea)))

           (assert that the value of
             (run* (x y) (conj2 (== 'pea y) (== 'split x)))
             is '((split pea)))

           (assert that the value of
             (run* (x y)
               (disj2 (conj2 (== 'split x) (== 'pea y))
                      (conj2 (== 'red x) (== 'bean y))))
             is '((split pea) (red bean)))

           (assert that the value of
             (run* r
               (fresh (x y)
                 (conj2 (disj2 (conj2 (== 'split x) (== 'pea y))
                               (conj2 (== 'red x) (== 'bean y)))
                        (== `(,x ,y soup) r))))
             is '((split pea soup) (red bean soup)))

           (assert that the value of
             (run* r
               (fresh (x y)
                 (disj2 (conj2 (== 'split x) (== 'pea y))
                        (conj2 (== 'red x) (== 'bean y)))
                 (== `(,x ,y soup) r)))
             is '((split pea soup) (red bean soup)))

           (assert that the value of
             (run* (x y z)
               (disj2 (conj2 (== 'split x) (== 'pea y))
                      (conj2 (== 'red x) (== 'bean y)))
               (== 'soup z))
             is '((split pea soup) (red bean soup)))

           (defrel (teacupo t)
             (disj2 (== 'tea t) (== 'cup t)))

           (assert that the value of
             (run* x (teacupo x))
             is '(tea cup))

           (assert that the value of
             (run* (x y)
               (disj2 (conj2 (teacupo x) (== #t y))
                      (conj2 (== #f x) (== #t y))))
             is '((#f #t) (tea #t) (cup #t)))

           (assert that the value of
             (run* (x y)
               (teacupo x)
               (teacupo y))
             is '((tea tea) (tea cup) (cup tea) (cup cup)))

           (assert that the value of
             (run* (x y)
               (teacupo x)
               (teacupo x))
             is '((tea _0) (cup _0)))

           (assert that the value of
             (run* (x y)
               (disj2 (conj2 (teacupo x) (teacupo x))
                      (conj2 (== #f x) (teacupo y))))
             is '((#f tea) (#f cup) (tea _0) (cup _0)))

           (assert that the value of
             (run* (x y)
               (conde ((== 'split x) (== 'pea y))
                      ((== 'red x) (== 'bean y))))
             is
             (run* (x y)
               (disj2 (conj2 (== 'split x) (== 'pea y))
                      (conj2 (== 'red x) (== 'bean y)))))

           (assert that the value of
             (run* x
               (disj2 (conj2 (== 'olive x) fail)
                      (== 'oil x)))
             is
             (run* x
               (conde ((== 'olive x) fail)
                      ((== 'oil x)))))

           (assert that the value of
             (run* (x y)
               (conde ((fresh (z) (== 'lentil z)))
                      ((== x y))))
             is '((_0 _1) (_0 _0)))

           (assert that the value of
             (run* (x y)
               (conde ((== 'split x) (== 'pea y))
                      ((== 'red x) (== 'bean y))
                      ((== 'green x) (== 'lentil y))))
             is '((split pea) (red bean) (green lentil)))

           ; Chapter 2

           (defrel (caro p a)
             (fresh (d)
               (== (cons a d) p)))

           (assert that the value of
             (run* q (caro '(a c o r n) q))
             is '(a))

           (assert that the value of
             (run* r (fresh (x y)
                       (caro `(,r ,y) x)
                       (== 'pear x)))
             is '(pear))

           (assert that the value of
             (run* r
               (fresh (x y)
                 (caro '(grape raisin pear) x)
                 (caro '((a) (b) (c)) y)
                 (== (cons x y) r)))
             is '((grape a)))

           (defrel (cdro p d)
             (fresh (a)
               (== (cons a d) p)))

           (assert that the value of
             (run* r
               (fresh (v)
                 (cdro '(a c o r n) v)
                 (fresh (w)
                   (cdro v w)
                   (caro w r))))
             is `(,(car (cdr (cdr '(a c o r n))))))

           (assert that the value of
             (run* q (cdro '(a c o r n) '(c o r n)))
             is '(_0))

           (assert that the value of
             (run* x (cdro '(c o r n) `(,x r n)))
             is '(o))

           (assert that the value of
             (run* l
               (fresh (x)
                 (cdro l '(c o r n))
                 (caro l x)
                 (== 'a x)))
             is '((a c o r n)))

           (defrel (conso a d p)
             (== `(,a . ,d) p))

           (assert that the value of
             (run* l (conso '(a b c) '(d e) l))
             is '(((a b c) d e)))

           (assert that the value of
             (run* x (conso x '(a b c) '(d a b c)))
             is '(d))

           (assert that the value of
             (run* r
               (fresh (x y z)
                 (== `(e a d ,x) r)
                 (conso y `(a ,z c) r)))
             is '((e a d c)))

           (assert that the value of
             (run* x (conso x `(a ,x c) `(d a ,x c)))
             is '(d))

           (assert that the value of
             (run* l
               (fresh (x)
                 (== `(d a ,x c) l)
                 (conso x `(a ,x c) l)))
             is '((d a d c)))

           (assert that the value of
             (run* x (conso x `(a ,x c) `(d a ,x c)))
             is '(d))

           (defrel (nullo x)
             (== '() x))

           (assert that the value of
             (run* q (nullo '(grape raisin pear)))
             is '())

           (assert that the value of
             (run* q (nullo '()))
             is '(_0))

           (assert that the value of
             (run* x (nullo x))
             is '(()))

           (defrel (pairo p)
             (fresh (a d)
               (conso a d p)))

           (assert that the value of
             (run* q (pairo (cons q q)))
             is '(_0))

           (assert that the value of
             (run* q (pairo '()))
             is '())

           (assert that the value of
             (run* q (pairo 'pair))
             is '())

           (assert that the value of
             (run* x (pairo x))
             is '((_0 . _1)))

           (assert that the value of
             (run* r (pairo (cons r '())))
             is '(_0))

           (defrel (singletono l)
             (fresh (d)
               (cdro l d)
               (nullo d)))

           (assert that the value of
             (run* x (singletono x))
             is '((_0)))

           ; Chapter 3

           (defrel (listo l)
             (conde
               ((nullo l))
               ((fresh (d)
                  (cdro l d)
                  (listo d)))))

           (assert that the value of
             (run* x (listo `(a b ,x d)))
             is '(_0))

           (assert that the value of
             (run 1 x (listo `(a b c . ,x)))
             is '(()))

           (assert that the value of
             (run 5 x (listo `(a b c . ,x)))
             is '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3)))

           (assert that the value of
             (run 7 (x y)
               (conde ((== 'split x) (== 'pea y))
                      ((== 'red x) (== 'bean y))
                      ((== 'green x) (== 'lentil y))))
             is '((split pea) (red bean) (green lentil)))

           (defrel (lolo l)
             (conde
               ((nullo l))
               ((fresh (a)
                 (caro l a)
                 (listo a))
                (fresh (d)
                  (cdro l d)
                  (lolo d)))))

           (assert that the value of
             (run* q
               (fresh (x y)
                 (lolo `((a b) (,x c) (d ,y)))))
             is '(_0))

           (assert that the value of
             (run 1 l (lolo l))
             is '(()))

           (assert that the value of
             (run 1 q
               (fresh (x)
                 (lolo `((a b) . ,x))))
             is '(_0))

           (assert that the value of
             (run 1 x (lolo `((a b) (c d) . ,x)))
             is '(()))

           (assert that the value of
             (run 5 x (lolo `((a b) (c d) . ,x)))
             is '(() (()) ((_0)) (() ()) ((_0 _1))))

           (assert that the value of
             (run 5 x (lolo x))
             is '(() (()) ((_0)) (() ()) ((_0 _1))))

           (defrel (loso l)
             (conde
               ((nullo l))
               ((fresh (a)
                 (caro l a)
                 (singletono a))
                (fresh (d)
                  (cdro l d)
                  (loso d)))))

           (assert that the value of
             (run 1 z (loso `((g) . ,z)))
             is '(()))

           (assert that the value of
             (run 5 z (loso `((g) . ,z)))
             is '(() ((_0)) ((_0) (_1)) ((_0) (_1) (_2)) ((_0) (_1) (_2) (_3))))

           (defrel (membero x l)
             (conde
               ((nullo l) fail)
               ((fresh (a)
                 (caro l x)))
               ((fresh (d)
                  (cdro l d)
                  (membero x d)))))

           (assert that the value of
             (run* q (membero 'olive '(virgin olive oil)))
             is '(_0))

           (assert that the value of
             (run 1 y (membero y '(hummus with pita)))
             is '(hummus))

           (assert that the value of
             (run* y (membero y '(hummus with pita)))
             is '(hummus with pita))

           (assert that the value of
             (run* y (membero y '(pear grape . peaches)))
             is '(pear grape))

           (assert that the value of
             (run* x (membero 'e `(pasta ,x fagioli)))
             is '(e))

           (assert that the value of
             (run 1 x (membero 'e `(pasta e ,x fagioli)))
             is '(_0))

           (assert that the value of
             (run 1 x (membero 'e `(pasta ,x e fagioli)))
             is '(e))

           (assert that the value of
             (run* (x y) (membero 'e `(pasta ,x fagioli ,y)))
             is '((e _0) (_0 e)))

           (assert that the value of
             (run* q
               (fresh (x y)
                 (== `(pasta ,x fagioli ,y) q)
                 (membero 'e q)))
             is '((pasta e fagioli _0) (pasta _0 fagioli e)))

           (assert that the value of
             (run 1 l (membero 'tofu l))
             is '((tofu . _0)))

           (assert that the value of
             (run 5 l (membero 'tofu l))
             is '((tofu . _0)
                  (_0 tofu . _1)
                  (_0 _1 tofu . _2)
                  (_0 _1 _2 tofu . _3)
                  (_0 _1 _2 _3 tofu . _4)))

           (defrel (proper-membero x l)
             (conde
               ((caro l x)
                (fresh (d)
                  (cdro l d)
                  (listo d)))
               ((fresh (d)
                  (cdro l d)
                  (proper-membero x d)))))

           (assert that the value of
             (run 12 l (proper-membero 'tofu l))
             is '((tofu)
                  (tofu _0)
                  (tofu _0 _1)
                  (_0 tofu)
                  (tofu _0 _1 _2)
                  (tofu _0 _1 _2 _3)
                  (_0 tofu _1)
                  (tofu _0 _1 _2 _3 _4)
                  (tofu _0 _1 _2 _3 _4 _5)
                  (_0 tofu _1 _2)
                  (tofu _0 _1 _2 _3 _4 _5 _6)
                  (_0 _1 tofu)))

           ; Chapter 4

           (defrel (appendo l t out)
             (conde
               ((nullo l) (== t out))
               ((fresh (a d res)
                  (conso a d l)
                  (appendo d t res)
                  (conso a res out)))))

           (assert that the value of
             (run 6 x
               (fresh (y z)
                 (appendo x y z)))
             is '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3) (_0 _1 _2 _3 _4)))

           (assert that the value of
             (run 3 y
               (fresh (x z)
                 (appendo x y z)))
             is '(_0 _0 _0))

           (assert that the value of
             (run 5 z
               (fresh (x y)
                 (appendo x y z)))
             is '(_0 (_0 . _1) (_0 _1 . _2) (_0 _1 _2 . _3) (_0 _1 _2 _3 . _4)))

           (assert that the value of
             (run* x (appendo '(cake) '(tastes yummy) x))
             is '((cake tastes yummy)))

           (assert that the value of
             (run* x (fresh (y) (appendo `(cake & ice ,y) '(tastes yummy) x)))
             is '((cake & ice _0 tastes yummy)))

           (assert that the value of
             (run* x (fresh (y) (appendo `(cake & ice cream) y x)))
             is '((cake & ice cream . _0)))

           (assert that the value of
             (run 1 x (fresh (y) (appendo `(cake & ice . ,y) '(d t) x)))
             is '((cake & ice d t)))

           (assert that the value of
             (run 5 x (fresh (y) (appendo `(cake & ice . ,y) '(d t) x)))
             is '((cake & ice d t)
                  (cake & ice _0 d t)
                  (cake & ice _0 _1 d t)
                  (cake & ice _0 _1 _2 d t)
                  (cake & ice _0 _1 _2 _3 d t)))

           (assert that the value of
             (run 5 y (fresh (x) (appendo `(cake & ice . ,y) '(d t) x)))
             is '(()
                  (_0)
                  (_0 _1)
                  (_0 _1 _2)
                  (_0 _1 _2 _3)))

           (assert that the value of
             (run 5 x (fresh (y) (appendo `(cake & ice . ,y) `(d t . ,y) x)))
             is '((cake & ice d t)
                  (cake & ice _0 d t _0)
                  (cake & ice _0 _1 d t _0 _1)
                  (cake & ice _0 _1 _2 d t _0 _1 _2)
                  (cake & ice _0 _1 _2 _3 d t _0 _1 _2 _3)))

           (assert that the value of
             (run* x (fresh (z) (appendo `(cake & ice cream) `(d t . ,z) x)))
             is '((cake & ice cream d t . _0)))

           (assert that the value of
             (run 6 x (fresh (y) (appendo x y '(cake & ice d t))))
             is '(() (cake) (cake &) (cake & ice) (cake & ice d) (cake & ice d t)))

           (assert that the value of
             (run 6 y (fresh (x) (appendo x y '(cake & ice d t))))
             is '((cake & ice d t)
                  (& ice d t)
                  (ice d t)
                  (d t)
                  (t)
                  ()))

           (assert that the value of
             (run 6 (x y) (appendo x y '(cake & ice d t)))
             is '((() (cake & ice d t))
                  ((cake) (& ice d t))
                  ((cake &) (ice d t))
                  ((cake & ice) (d t))
                  ((cake & ice d) (t))
                  ((cake & ice d t) ())))

           (defrel (appendo l t out)
             (conde
               ((nullo l) (== t out))
               ((fresh (a d res)
                  (conso a d l)
                  (conso a res out)
                  (appendo d t res)))))

           (assert that the value of
             (run* (x y) (appendo x y '(cake & ice d t)))
             is '((() (cake & ice d t))
                  ((cake) (& ice d t))
                  ((cake &) (ice d t))
                  ((cake & ice) (d t))
                  ((cake & ice d) (t))
                  ((cake & ice d t) ())))

           (defrel (unwrapo x out)
             (conde
               ((fresh (a)
                  (caro x a)
                  (unwrapo a out)))
               ((== x out))))

           (assert that the value of
             (run* x (unwrapo '(((pizza))) x))
             is '((((pizza)))
                  ((pizza))
                  (pizza)
                  pizza))

           (assert that the value of
             (run 1 x (unwrapo 'pizza x))
             is '(pizza))

           (assert that the value of
             (run 1 x (unwrapo `((,x)) 'pizza))
             is '(pizza))

           (assert that the value of
             (run 4 x (unwrapo `((,x)) 'pizza))
             is '(pizza
                  (pizza . _0)
                  ((pizza . _0) . _1)
                  (((pizza . _0) . _1) . _2)))

           (assert that the value of
             (run 4 x (unwrapo x '((pizza))))
             is '(((pizza))
                  (((pizza)) . _0)
                  ((((pizza)) . _0) . _1)
                  (((((pizza)) . _0) . _1) . _2)))

           ; Chapter 5

           (defrel (memo x l out)
             (conde
               ((caro l x) (== l out))
               ((fresh (d)
                  (cdro l d)
                  (memo x d out)))))

           (assert that the value of
             (run* q (memo 'fig '(pea) '(pea)))
             is '())

           (assert that the value of
             (run* out (memo 'fig '(fig) out))
             is '((fig)))

           (assert that the value of
             (run* out (memo 'fig '(fig pea) out))
             is '((fig pea)))

           (assert that the value of
             (run* r (memo r '(roll okra fig beet fig pea)
                             '(fig beet fig pea)))
             is '(fig))

           (assert that the value of
             (run* x (memo 'fig '(fig pea) `(pea ,x)))
             is '())

           (assert that the value of
             (run* x (memo 'fig '(fig pea) `(,x pea)))
             is '(fig))

           (assert that the value of
             (run* out (memo 'fig '(beet fig pea) out))
             is '((fig pea)))

           (assert that the value of
             (run 1 out (memo 'fig '(fig fig pea) out))
             is '((fig fig pea)))

           (assert that the value of
             (run* out (memo 'fig '(fig fig pea) out))
             is '((fig fig pea) (fig pea)))

           (assert that the value of
             (run* out (fresh (x) (memo 'fig `(a ,x c fig e) out)))
             is '((fig c fig e) (fig e)))

           (assert that the value of
             (run 5 (x y) (memo 'fig `(fig d fig e . ,y) x))
             is '(((fig d fig e . _0) _0)
                  ((fig e . _0) _0)
                  ((fig . _0) (fig . _0))
                  ((fig . _0) (_1 fig . _0))
                  ((fig . _0) (_1 _2 fig . _0))))

           (defrel (rembero x l out)
             (conde
              ((nullo l) (== out '()))
              ((conso x out l))
              ((fresh (a d res)
                 (conso a d l)
                 (conso a res out)
                 (rembero x d res)))))

           (assert that the value of
             (run* out (rembero 'pea '(pea) out))
             is '(() (pea)))

           (assert that the value of
             (run* out (rembero 'pea '(pea pea) out))
             is '((pea) (pea) (pea pea)))

           (assert that the value of
             (run* out (fresh (y z) (rembero y `(a b ,y d ,z e) out)))
             is '((b a d _0 e)
                  (a b d _0 e)
                  (a b d _0 e)
                  (a b d _0 e)
                  (a b _0 d e)
                  (a b e d _0)
                  (a b _0 d _1 e)))

           (assert that the value of
             (run* (y z) (rembero y `(,y d ,z e) `(,y d e)))
             is '((d d) (d d) (_0 _0) (e e)))

           (assert that the value of
             (run 4 (y z w out) (rembero y `(,z . ,w) out))
             is '((_0 _0 _1 _1)
                  (_0 _1 () (_1))
                  (_0 _1 (_0 . _2) (_1 . _2))
                  (_0 _1 (_2) (_1 _2))))

           ; Chapter 6

           (defrel (alwayso)
             (conde (succeed)
                    ((alwayso))))

           (assert that the value of
             (run 1 q (alwayso))
             is '(_0))

           (assert that the value of
             (run 1 q (conde (succeed) ((alwayso))))
             is '(_0))

           (assert that the value of
             (run 2 q (alwayso))
             is '(_0 _0))

           (assert that the value of
             (run 2 q succeed)
             is '(_0))

           (assert that the value of
             (run 5 q (== 'onion q) (alwayso))
             is '(onion onion onion onion onion))

           (assert that the value of
             (run 1 q
               (== 'garlic q)
               succeed
               (== 'onion q))
             is '())

           (assert that the value of
             (run 1 q
               (conde
                 ((== 'garlic q) (alwayso))
                 ((== 'onion q)))
               (== 'onion q))
             is '(onion))

           (assert that the value of
             (run 5 q
               (conde
                 ((== 'garlic q) (alwayso))
                 ((== 'onion q) (alwayso)))
               (== 'onion q))
             is '(onion onion onion onion onion))

           (defrel (nevero)
             (nevero))

           (assert that the value of
             (run 1 q fail (nevero))
             is '())

           (assert that the value of
             (run 1 q (conde (succeed) ((nevero))))
             is '(_0))

           (assert that the value of
             (run 1 q (conde ((nevero)) (succeed)))
             is '(_0))

           (assert that the value of
             (run 5 q (conde ((nevero))
                             ((alwayso))
                             ((nevero))))
             is '(_0 _0 _0 _0 _0))

           (assert that the value of
             (run 6 q (conde ((== 'spicy q) (nevero))
                             ((== 'hot q) (nevero))
                             ((== 'apple q) (alwayso))
                             ((== 'cider q) (alwayso))))
             is '(apple cider apple cider apple cider))

           ; Chapter 7

           (defrel (bit-xoro x y r)
             (conde ((== 0 x) (== 0 y) (== 0 r))
                    ((== 0 x) (== 1 y) (== 1 r))
                    ((== 1 x) (== 0 y) (== 1 r))
                    ((== 1 x) (== 1 y) (== 0 r))))

           (assert that the value of
             (run* (x y) (bit-xoro x y 0))
             is '((0 0) (1 1)))

           (assert that the value of
             (run* (x y) (bit-xoro x y 1))
             is '((0 1) (1 0)))

           (assert that the value of
             (run* (x y z) (bit-xoro x y z))
             is '((0 0 0) (0 1 1) (1 0 1) (1 1 0)))

           (defrel (bit-ando x y r)
             (conde ((== 0 x) (== 0 y) (== 0 r))
                    ((== 0 x) (== 1 y) (== 0 r))
                    ((== 1 x) (== 0 y) (== 0 r))
                    ((== 1 x) (== 1 y) (== 1 r))))

           (assert that the value of
             (run* (x y) (bit-ando x y 1))
             is '((1 1)))

           (defrel (half-addero x y r c)
             (bit-xoro x y r)
             (bit-ando x y c))

           (assert that the value of
             (run* r (half-addero 1 1 r 1))
             is '(0))

           (assert that the value of
             (run* (x y r c) (half-addero x y r c))
             is '((0 0 0 0) (0 1 1 0) (1 0 1 0) (1 1 0 1)))

           (defrel (full-addero b x y r c)
             (fresh (w xy wz)
               (half-addero x y w xy)
               (half-addero w b r wz)
               (bit-xoro xy wz c)))

           (assert that the value of
             (run* (r c) (full-addero 0 1 1 r c))
             is '((0 1)))

           (assert that the value of
             (run* (r c) (full-addero 1 1 1 r c))
             is '((1 1)))

           (assert that the value of
             (run* (b x y r c) (full-addero b x y r c))
             is '((0 0 0 0 0)
                  (1 0 0 1 0)
                  (0 0 1 1 0)
                  (1 0 1 0 1)
                  (0 1 0 1 0)
                  (1 1 0 0 1)
                  (0 1 1 0 1)
                  (1 1 1 1 1)))

           (define (build-num n)
             (cond ((odd? n) (cons 1 (build-num (/ (- n 1) 2))))
                   ((and (not (zero? n)) (even? n))
                    (cons 0 (build-num (/ n 2))))
                   ((zero? n) '())))

           (assert that the value of
             (build-num 0) is '())

           (assert that the value of
             (build-num 36) is '(0 0 1 0 0 1))

           (assert that the value of
             (build-num 19) is '(1 1 0 0 1))

           (defrel (poso n)
             (fresh (a d)
              (== `(,a . ,d) n)))

           (defrel (>1o n)
             (fresh (a ad dd)
               (== `(,a ,ad . ,dd) n)))

           (defrel (addero b n m r)
             (conde
               ((== 0 b) (== '() m) (== n r))
               ((== 0 b) (== '() n) (== m r) (poso m))
               ((== 1 b) (== '() m) (addero 0 n '(1) r))
               ((== 1 b) (== '() n) (poso m) (addero 0 '(1) m r))
               ((== '(1) n) (== '(1) m)
                (fresh (a c)
                  (== `(,a ,c) r)
                  (full-addero b 1 1 a c)))
               ((== '(1) n) (gen-addero b n m r))
               ((== '(1) m) (>1o n) (>1o r) (addero b '(1) n r))
               ((>1o n) (gen-addero b n m r))))

           (defrel (gen-addero b n m r)
             (fresh (a c d e x y z)
               (== `(,a . ,x) n)
               (== `(,d . ,y) m) (poso y)
               (== `(,c . ,z) r) (poso z)
               (full-addero b a d c e)
               (addero e x y z)))

           (assert that the value of
             (run 3 (x y r) (addero 0 x y r))
             is '((_0 () _0)
                  (() (_0 . _1) (_0 . _1))
                  ((1) (1) (0 1))))

           ; the order of results seems to be different than in the book
           (assert that the value of
             (run 20 (x y r) (addero 0 x y r))
             is '((_0 () _0)
                  (() (_0 . _1) (_0 . _1))
                  ((1) (1) (0 1))
                  ((1) (0 _0 . _1) (1 _0 . _1))
                  ((1) (1 1) (0 0 1))

                  ((0 _0 . _1) (1) (1 _0 . _1))
                  ((1) (1 0 _0 . _1) (0 1 _0 . _1))
                  ((1) (1 1 1) (0 0 0 1))
                  ((1 1) (1) (0 0 1))
                  ((1) (1 1 0 _0 . _1) (0 0 1 _0 . _1))

                  ((0 1) (0 1) (0 0 1))
                  ((1) (1 1 1 1) (0 0 0 0 1))
                  ((1 0 _0 . _1) (1) (0 1 _0 . _1))
                  ((1) (1 1 1 0 _0 . _1) (0 0 0 1 _0 . _1))
                  ((1) (1 1 1 1 1) (0 0 0 0 0 1))

                  ((1 1 1) (1) (0 0 0 1))
                  ((1) (1 1 1 1 0 _0 . _1) (0 0 0 0 1 _0 . _1))
                  ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1))
                  ((1 1 0 _0 . _1) (1) (0 0 1 _0 . _1))
                  ((0 1) (1 1) (1 0 1))))

           (assert that the value of
             (run* s (gen-addero 1 '(0 1 1) '(1 1) s))
             is '((0 1 0 1)))

           ; the order of the last two results are swapped in the book
           (assert that the value of
             (run* (x y) (addero 0 x y '(1 0 1)))
             is '(((1 0 1) ())
                  (() (1 0 1))
                  ((1) (0 0 1))
                  ((0 0 1) (1))
                  ((0 1) (1 1))
                  ((1 1) (0 1))))

           (defrel (+o n m k)
             (addero 0 n m k))

           (defrel (-o n m k)
             (+o m k n))

           (assert that the value of
             (run* q (-o '(0 0 0 1) '(1 0 1) q))
             is '((1 1)))

           (assert that the value of
             (run* q (-o '(0 1 1) '(0 1 1) q))
             is '(()))

           (assert that the value of
             (run* q (-o '(0 1 1) '(0 0 0 1) q))
             is '())

           (defrel (lengtho l n)
             (conde
               ((nullo l) (== '() n))
               ((fresh (d res)
                  (cdro l d)
                  (+o '(1) res n)
                  (lengtho d res)))))

           (assert that the value of
             (run 1 n (lengtho '(jicama rhubarb guava) n))
             is '((1 1)))

           (assert that the value of
             (run* ls (lengtho ls '(1 0 1)))
             is '((_0 _1 _2 _3 _4)))

           (assert that the value of
             (run* q (lengtho '(1 0 1) 3))
             is '())

           (assert that the value of
             (run 3 q (lengtho q q))
             is '(() (1) (0 1)))

           ; Chapter 8

           (defrel (*o n m p)
             (conde
               ((== '() n) (== '() p))
               ((poso n) (== '() m) (== '() p))
               ((== '(1) n) (poso m) (== m p))
               ((>1o n) (== '(1) m) (== n p))
               ((fresh (x z)
                  (== `(0 . ,x) n) (poso x)
                  (== `(0 . ,z) p) (poso z)
                  (>1o m)
                  (*o x m z)))
               ((fresh (x y)
                  (== `(1 . ,x) n) (poso x)
                  (== `(0 . ,y) m) (poso y)
                  (*o m n p)))
               ((fresh (x y)
                  (== `(1 . ,x) n) (poso x)
                  (== `(1 . ,y) m) (poso y)
                  (odd-*o x n m p)))))

           (defrel (odd-*o x n m p)
             (fresh (q)
               (bound-*o q p n m)
               (*o x m q)
               (+o `(0 . ,q) m p)))

           (defrel (bound-*o q p n m)
             (conde
               ((== '() q) (poso p))
               ((fresh (a0 a1 a2 a3 x y z)
                  (== `(,a0 . ,x) q)
                  (== `(,a1 . ,y) p)
                  (conde
                    ((== '() n)
                     (== `(,a2 . ,z) m)
                     (bound-*o x y z '()))
                    ((== `(,a3 . ,z) n)
                     (bound-*o x y z m)))))))

           (assert that the value of
             (run 10 (x y r) (*o x y r))
             is '((() _0 ())
                  ((_0 . _1) () ())
                  ((1) (_0 . _1) (_0 . _1))
                  ((_0 _1 . _2) (1) (_0 _1 . _2))
                  ((0 1) (_0 _1 . _2) (0 _0 _1 . _2))
                  ((0 0 1) (_0 _1 . _2) (0 0 _0 _1 . _2))
                  ((1 _0 . _1) (0 1) (0 1 _0 . _1))
                  ((0 0 0 1) (_0 _1 . _2) (0 0 0 _0 _1 . _2))
                  ((1 _0 . _1) (0 0 1) (0 0 1 _0 . _1))
                  ((0 1 _0 . _1) (0 1) (0 0 1 _0 . _1))))

           (assert that the value of
             (run* (x y r)
               (== `(,x ,y ,r) '((1 1) (1 1) (1 0 0 1)))
               (*o x y r))
             is '(((1 1) (1 1) (1 0 0 1))))

           (assert that the value of
             (run 1 (n m) (*o n m '(1)))
             is '(((1) (1))))

           (assert that the value of
             (run 1 (n m)
               (>1o n)
               (>1o m)
               (*o n m '(1 1)))
             is '())

           (assert that the value of
             (run* p (*o '(1 1 1) '(1 1 1 1 1 1) p))
             is '((1 0 0 1 1 1 0 1 1)))

           (defrel (=lo n m)
             (conde
               ((== '() n) (== '() m))
               ((== '(1) n) (== '(1) m))
               ((fresh (a x b y)
                  (== `(,a . ,x) n) (poso x)
                  (== `(,b . ,y) m) (poso y)
                  (=lo x y)))))

           (assert that the value of
             (run* (w x y) (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1)))
             is '((_0 _1 (_2 1))))

           (assert that the value of
             (run* b (=lo '(1) `(,b)))
             is '(1))

           (assert that the value of
             (run* n (=lo `(1 0 1 . ,n) '(0 1 1 0 1)))
             is '((_0 1)))

           (assert that the value of
             (run 5 (y z) (=lo `(1 . ,y) `(1 . ,z)))
             is '((() ())
                  ((1) (1))
                  ((_0 1) (_1 1))
                  ((_0 _1 1) (_2 _3 1))
                  ((_0 _1 _2 1) (_3 _4 _5 1))))

           (defrel (<lo n m)
             (conde
               ((== '() n) (poso m))
               ((== '(1) n) (>1o m))
               ((fresh (a x b y)
                  (== `(,a . ,x) n) (poso x)
                  (== `(,b . ,y) m) (poso y)
                  (<lo x y)))))

           (assert that the value of
             (run 7 (y z) (<lo `(1 . ,y) `(0 1 1 0 1 . ,z)))
             is '((() _0)
                  ((1) _0)
                  ((_0 1) _1)
                  ((_0 _1 1) _2)
                  ((_0 _1 _2 1) (_3 . _4))
                  ((_0 _1 _2 _3 1) (_4 _5 . _6))
                  ((_0 _1 _2 _3 _4 1) (_5 _6 _7 . _8))))

           (defrel (<=lo n m)
             (conde
               ((=lo n m))
               ((<lo n m))))

           (assert that the value of
             (run 7 (n m) (<=lo n m))
             is '((() ())
                  ((1) (1))
                  (() (_0 . _1))
                  ((_0 1) (_1 1))
                  ((1) (_0 _1 . _2))
                  ((_0 _1 1) (_2 _3 1))
                  ((_0 1) (_1 _2 _3 . _4))))

           (assert that the value of
             (run 5 (n m)
               (<=lo n m)
               (*o n '(0 1) m))
             is '((() ())
                  ((1) (0 1))
                  ((0 1) (0 0 1))
                  ((1 1) (0 1 1))
                  ((1 _0 1) (0 1 _0 1))))

           (defrel (<o n m)
             (conde
               ((<lo n m))
               ((=lo n m)
                (fresh (x)
                  (poso x)
                  (+o n x m)))))

           (defrel (<=o n m)
             (conde
               ((== n m))
               ((<o n m))))

           (assert that the value of
             (run* q (<o '(1 0 1) '(1 1 1)))
             is '(_0))

           (assert that the value of
             (run* q (<o '(1 1 1) '(1 0 1)))
             is '())

           (assert that the value of
             (run* q (<o '(1 0 1) '(1 0 1)))
             is '())

           (assert that the value of
             (run* q (<=o '(1 0 1) '(1 0 1)))
             is '(_0))

           (assert that the value of
             (run* n (<o n '(1 0 1)))
             is '(() (1) (_0 1) (0 0 1)))

           (assert that the value of
             (run* m (<o '(1 0 1) m))
             is '((_0 _1 _2 _3 . _4) (0 1 1) (1 1 1)))

           (defrel (/o n m q r)
             (conde
               ((== '() q) (== n r) (<o n m))
               ((== '(1) q) (== '() r) (== n m) (<o r m))
               ((<o m n) (<o r m)
                (fresh (mq)
                  (<=lo mq n)
                  (*o m q mq)
                  (+o mq r n)))))

           (assert that the value of
             (run 4 (n m q r) (/o n m q r))
             is '((() (_0 . _1) () ())
                  ((1) (_0 _1 . _2) () (1))
                  ((_0 . _1) (_0 . _1) (1) ())
                  ((_0 1) (_1 _2 _3 . _4) () (_0 1))))

           (assert that the value of
             (run* m (fresh (r) (/o '(1 0 1) m '(1 1 1) r)))
             is '())

           (defrel (splito n r l h)
             (conde
               ((== '() n) (== '() h) (== '() l))
               ((fresh (b ^n)
                  (== `(0 ,b . ,^n) n) (== '() r)
                  (== `(,b . ,^n) h) (== '() l)))
               ((fresh (^n)
                  (== `(1 . ,^n) n) (== '() r)
                  (== ^n h) (== '(1) l)))
               ((fresh (b ^n a ^r)
                  (== `(0 ,b . ,^n) n)
                  (== `(,a . ,^r) r) (== '() l)
                  (splito `(,b . ,^n) ^r '() h)))
               ((fresh (^n a ^r)
                  (== `(1 . ,^n) n)
                  (== `(,a . ,^r) r) (== '(1) l)
                  (splito ^n ^r '() h)))
               ((fresh (b ^n a ^r ^l)
                  (== `(,b . ,^n) n)
                  (== `(,a . ,^r) r)
                  (== `(,b . ,^l) l)
                  (poso ^l)
                  (splito ^n ^r ^l h)))))

           (assert that the value of
             (run* (l h) (splito '(0 0 1 0 1) '() l h))
             is '((() (0 1 0 1))))

           (assert that the value of
             (run* (l h) (splito '(0 0 1 0 1) '(1) l h))
             is '((() (1 0 1))))

           (assert that the value of
             (run* (l h) (splito '(0 0 1 0 1) '(0 1) l h))
             is '(((0 0 1) (0 1))))

           (assert that the value of
             (run* (l h) (splito '(0 0 1 0 1) '(1 1) l h))
             is '(((0 0 1) (0 1))))

           (assert that the value of
             (run* (r l h) (splito '(0 0 1 0 1) r l h))
             is '((() () (0 1 0 1))
                  ((_0) () (1 0 1))
                  ((_0 _1) (0 0 1) (0 1))
                  ((_0 _1 _2) (0 0 1) (1))
                  ((_0 _1 _2 _3) (0 0 1 0 1) ())
                  ((_0 _1 _2 _3 _4 . _5) (0 0 1 0 1) ())))

           (defrel (/o n m q r)
             (conde
               ((== '() q) (== r n) (<o n m))
               ((== '(1) q) (=lo m n) (+o r m n) (<o r m))
               ((poso q) (<lo m n) (<o r m)
                (n-wider-than-mo n m q r))))

           (defrel (n-wider-than-mo n m q r)
             (fresh (n_high n_low q_high q_low)
               (fresh (mq_low mrq_low rr r_high)
                 (splito n r n_low n_high)
                 (splito q r q_low q_high)
                 (conde
                   ((== '() n_high)
                    (== '() q_high)
                    (-o n_low r mq_low)
                    (*o m q_low mq_low))
                   ((poso n_high)
                    (*o m q_low mq_low)
                    (+o r mq_low mrq_low)
                    (-o mrq_low n_low rr)
                    (splito rr r '() r_high)
                    (/o n_high m q_high r_high))))))

           (assert that the value of
             (run 1 (y z) (/o `(1 0 . ,y) '(0 1) z '()))
             is '())

           (defrel (logo n b q r)
             (conde
               ((== '() q) (<=o n b) (+o r '(1) n))
               ((== '(1) q) (>1o b) (=lo n b) (+o r b n))
               ((== '(1) b) (poso q) (+o r '(1) n))
               ((== '() b) (poso q) (== r n))
               ((== '(0 1) b)
                (fresh (a ad dd)
                  (poso dd)
                  (== `(,a ,ad . ,dd) n)
                  (exp2o n '() q)
                  (fresh (s)
                    (splito n dd r s))))
               ((<=o '(1 1) b) (<lo b n)
                (base-three-or-moreo n b q r))))

           (defrel (exp2o n b q)
             (conde
               ((== '(1) n) (== '() q))
               ((>1o n) (== '(1) q)
                (fresh (s)
                  (splito n b s '(1))))
               ((fresh (q1 b2)
                  (== `(0 . ,q1) q) (poso q1)
                  (<lo b n)
                  (appendo b `(1 . ,b) b2)
                  (exp2o n b2 q1)))
               ((fresh (q1 n_high b2 s)
                  (== `(1 . ,q1) q) (poso q1)
                  (poso n_high)
                  (splito n b s n_high)
                  (appendo b `(1 . ,b) b2)
                  (exp2o n_high b2 q1)))))

           (defrel (base-three-or-moreo n b q r)
             (fresh (bw1 bw nw nw1 q_low1 q_low s)
                (exp2o b '() bw1)
                (+o bw1 '(1) bw)
                (<lo q n)
                (fresh (q1 bwq1)
                  (+o q '(1) q1)
                  (*o bw q1 bwq1)
                  (<o nw1 bwq1))
                (exp2o n '() nw1)
                (+o nw1 '(1) nw)
                (/o nw bw q_low1 s)
                (+o q_low '(1) q_low1)
                (<=lo q_low q)
                (fresh (bq_low q_high s qd_high qd)
                  (repeated-mulo b q_low bq_low)
                  (/o nw bw1 q_high s)
                  (+o q_low qd_high q_high)
                  (+o q_low qd q)
                  (<=o qd qd_high)
                  (fresh (bqd bq1 bq)
                    (repeated-mulo b qd bqd)
                    (*o bq_low bqd bq)
                    (*o b bq bq1)
                    (+o bq r n)
                    (<o n bq1)))))

           (defrel (repeated-mulo n q nq)
             (conde
               ((poso n) (== '() q) (== '(1) nq))
               ((== '(1) q) (== n nq))
               ((>1o q)
                (fresh (q1 nq1)
                  (+o q1 '(1) q)
                  (repeated-mulo n q1 nq1)
                  (*o nq1 n nq)))))

           (assert that the value of
             (run* r (logo '(0 1 1 1) '(0 1) '(1 1) r))
             is '((0 1 1)))

           '(let ()
             (display "This may take a while...") (newline)
              (assert that the value of
                (run 9 (b q r)
                  (logo '(0 0 1 0 0 0 1) b q r)
                  (>1o q))
                is '((() (_0 _1 . _2) (0 0 1 0 0 0 1)) ; 68 = 0^n + 68, where n > 1
                     ((1) (_0 _1 . _2) (1 1 0 0 0 0 1)); 68 = 1^n + 67, where n > 1
                     ((0 1) (0 1 1) (0 0 1))           ; 68 = 2^6 + 4
                     ((1 1) (1 1) (1 0 0 1 0 1))       ; 68 = 3^3 + 41
                     ((0 0 1) (1 1) (0 0 1))           ; 68 = 4^3 + 4
                     ((0 0 0 1) (0 1) (0 0 1))         ; 68 = 8^2 + 4
                     ((1 0 1) (0 1) (1 1 0 1 0 1))     ; 68 = 5^2 + 43
                     ((0 1 1) (0 1) (0 0 0 0 0 1))     ; 68 = 6^2 + 32
                     ((1 1 1) (0 1) (1 1 0 0 1)))))     ; 68 = 7^2 + 19

           (defrel (expo b q n)
             (logo n b q '()))

           '(let ()
              (display "This may take a while...") (newline)
              (assert that the value of
                (run* t (expo '(1 1) '(1 0 1) t))
                is `(,(build-num 243))))))

       (run-tests)))
