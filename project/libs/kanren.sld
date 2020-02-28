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
                        (write val)
                        (let ((result exp))
                          (if (equal? result val)
                              (begin (display " : OK")
                                     (newline))
                              (begin (display " : FAIL ... got ")
                                     (write result)
                                     (display " instead.")
                                     (newline)
                                     (error "ASSERTION FAILED")))))))))

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
            is (run* q (== q 'pea)))

          ; TODO: What is going on here?

          (display (equal? (run* q (== q 'pea))
                           (run* p (== p 'pea))))
          (let ((a (run* q (== q 'pea)))
                (b (run* q (== q 'pea))))
            (display (equal? a b)))))

      (run-tests)))
