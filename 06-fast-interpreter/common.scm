
(define-library (06-fast-interpreter common)

  (export activation-frame-argument
          activation-frame-argument-length
          allocate-activation-frame
          compute-kind
          definitial
          description-extend!
          environment-next
          g.current
          g.current-extend!
          g.init
          g.init-extend!
          get-description
          global-fetch
          global-update!
          listify!
          predefined-fetch
          r.init
          r-extend*
          sg.current
          sg.init
          sr.init
          sr-extend*
          set-activation-frame-argument!
          static-wrong
          undefined-value)

  (import (builtin core)
          (libs utils)
          (libs book))

  (begin
    (define (static-wrong message . culprits)
      (println `(*static-error* ,message . ,culprits))
      (lambda _args
        (apply wrong (cons message culprits))))

    (define (deep-fetch sr i j)
      (if (= i 0)
          (activation-frame-argument sr j)
          (deep-fetch (environment-next sr) (- i 1) j)))

    (define (deep-update! sr i j v)
      (if (= i 0)
          (set-activation-frame-argument! sr j v)
          (deep-update! (environment-next sr) (- i 1) j v)))

    (define (r-extend* r n*)
      (cons n* r))

    (define g.current '())
    (define g.init '())

    (define (sr-extend* sr v*)
      (set-environment-next! v* sr)
      v*)

    (define r.init '())
    (define sr.init '())

    (define (compute-kind r n)
      (or (local-variable? r 0 n)
          (global-variable? g.current n)
          (global-variable? g.init n)))

    (define (g.current-extend! n)
      (let ((level (length g.current)))
        (set! g.current (cons (cons n `(global . ,level)) g.current))
        level))

    (define (g.init-extend! n)
      (let ((level (length g.init)))
        (set! g.init (cons (cons n `(predefined . ,level)) g.init))
        level))

    (define (global-variable? g n)
      (let ((var (assq n g)))
        (and (pair? var) (cdr var))))

    (define (local-variable? r i n)
      (define (scan names j)
        (cond ((pair? names) (if (eq? n (car names))
                                 `(local ,i . ,j)
                                 (scan (cdr names) (+ 1 j))))
              ((null? names) (local-variable? (cdr r) (+ i 1) n))
              ((eq? n names) `(local ,i . ,j))))

      (and (pair? r)
           (scan (car r) 0)))

    (define (listify! v* arity)
      (define (loop index result)
        (if (= arity index)
            (set-activation-frame-argument! v* arity result)
            (loop (- index 1)
                  (cons (activation-frame-argument v* (- index 1))
                        result))))
      (loop (- (activation-frame-argument-length v*) 1) '()))

    (define (definitial name value)
      (g.init-initialize! name value))

    (define sg.current (make-vector 100))
    (define sg.init (make-vector 100))
    (define (global-fetch i)
      (vector-ref sg.current i))
    (define (global-update! i v)
      (vector-set! sg.current i v))
    (define (predefined-fetch i)
      (vector-ref sg.init i))

    (define (g.current-initialize! name)
      (let ((kind (compute-kind r.init name)))
        (if kind
            (case (car kind)
              ((global)
               (vector-set! sg.current (cdr kind) undefined-value))
              (else (static-wrong "Wrong redefinition" name)))
            (let ((index (g.current-extend! name)))
              (vector-set! sg.current index undefined-value))))
      name)

    (define (g.init-initialize! name value)
      (let ((kind (compute-kind r.init name)))
        (if kind
            (case (car kind)
              ((predefined)
               (vector-set! sg.init (cdr kind) value))
              (else (static-wrong "Wrong redefinition" name)))
            (let ((index (g.init-extend! name)))
              (vector-set! sg.init index value))))
      name)

    (define desc.init '())
    (define (description-extend! name description)
      (set! desc.init (cons (cons name description) desc.init))
      name)
    (define (get-description name)
      (let ((p (assq name desc.init)))
        (and (pair? p) (cdr p))))

    (define undefined-value '*undefined*)

    (g.current-initialize! 'x)
    (g.current-initialize! 'y)
    (g.current-initialize! 'z)
    (g.current-initialize! 'foo)
    (g.current-initialize! 'bar)

    ; The following implementations are not defined in the book, so I'm making them up...

    (define (environment-next frame)
      (car frame))

    (define (set-environment-next! frame sr)
      (set-car! frame sr))

    (define (allocate-activation-frame size)
      (list '() (make-vector size) size))

    (define (activation-frame-argument frame index)
      (vector-ref (cadr frame) index))

    (define (set-activation-frame-argument! frame index value)
      (vector-set! (cadr frame) index value))

    (define (activation-frame-argument-length frame)
      (caddr frame))))
