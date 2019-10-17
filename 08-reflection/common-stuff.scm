
(define g.current '())
(define g.init '())
(define desc.init '())
(define r.init '())
(define sr.init '())
(define *dynamic-variables* '())

(define sg.current (make-vector 100))
(define sg.init (make-vector 100))
(define sg.current.names '())

; ============================================================================
;  Chapter 8 additions

(define (make-reified-environment sr r)
  (list 'reified-environment sr r))

(define (reified-environment? obj)
  (and (pair? obj) (eq? (car obj) 'reified-environment)))

(define (reified-environment-sr obj)
  (cadr obj))

(define (reified-environment-r obj)
  (caddr obj))


(define (r-extend* r n*)
  (let ((old-r (bury-r r 1)))
    (define (scan n* i)
      (cond ((pair? n*) (cons (list (car n*) `(local 0 . ,i))
                              (scan (cdr n*) (+ i 1))))
            ((null? n*) old-r)
            (else (cons (list n* `(local 0 . ,i)) old-r))))
    (scan n* 0)))

(define (bury-r r offset)
  (map (lambda (d)
         (let ((name (car d))
               (type (caadr d)))
           (case type
             ((local checked-local)
              (let* ((addr (cadr d))
                     (i (cadr addr))
                     (j (cddr addr)))
                `(,name (,type ,(+ i offset) . ,j) . ,(cddr d))))
             (else d))))
       r))

(define (extract-addresses n* r)
  (if (null? n*)
      r
      (begin (define (scan n*)
               (if (pair? n*)
                   (cons (list (car n*) (compute-kind r (car n*)))
                         (scan (cdr n*)))
                   '()))
             (scan n*))))


(define (variable-value-lookup kind sr)
  (if (pair? kind)
      (case (car kind)
        ((checked-local)
         (let ((i (cadr kind))
               (j (cddr kind)))
           (set! *val* (deep-fetch sr i j))
           (if (eq? *val* undefined-value)
               (signal-exception #t (list "Uninitialized local variable")))))
        ((local)
         (let ((i (cadr kind))
               (j (cddr kind)))
           (set! *val* (if (= i 0)
                           (activation-frame-argument sr j)
                           (deep-fetch sr i j)))))
        ((shadowable)
         (let ((i (cadr kind))
               (j (cddr kind)))
           (shadowable-fetch sr i j)))
        ((global)
         (let ((i (cdr kind)))
           (set! *val* (global-fetch i))
           (if (eq? *val* undefined-value)
               (signal-exception #t (list "Uninitialized global variable")))))
        ((predefined)
         (let ((i (cdr kind)))
           (set! *val* (predefined-fetch i)))))
      (signal-exception #f (list "No such variable"))))

(define (variable-value-update! kind sr v)
  (if (pair? kind)
      (case (car kind)
        ((checked-local)
         (let ((i (cadr kind))
               (j (cddr kind)))
           (deep-update sr i j v)))
        ((local)
         (let ((i (cadr kind))
               (j (cddr kind)))
           (set! *val* (if (= i 0)
                           (set-activation-frame-argument! sr j v)
                           (deep-update! sr i j v)))))
        ((shadowable)
         (let ((i (cadr kind))
               (j (cddr kind)))
           (shadowable-update! sr i j v)))
        ((global)
         (let ((i (cdr kind)))
           (global-update! i v)))
        ((predefined)
         (signal-exception #f (list "Immutable variable"))))
      (signal-exception #f (list "No such variable"))))

; ============================================================================

(define (definitial name value)
  (g.init-initialize! name value))

(define (static-wrong message . culprits)
  (println `(*static-error* ,message . ,culprits))
  (lambda _args
    (apply wrong (cons message culprits))))

(define (install-code! code)
  (let ((start (vector-length *code*)))
    (set! *code* (vector-append *code* code))
    start))

; ============================================================================

(define (deep-fetch sr i j)
  (if (= i 0)
      (activation-frame-argument sr j)
      (deep-fetch (environment-next sr) (- i 1) j)))

(define (deep-update! sr i j v)
  (if (= i 0)
      (set-activation-frame-argument! sr j v)
      (deep-update! (environment-next sr) (- i 1) j v)))

(define (sr-extend* sr v*)
  (set-environment-next! v* sr)
  v*)

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
          (else (static-wrong "Wrong redefinition" name g.current)))
        (let ((index (g.init-extend! name)))
          (vector-set! sg.init index value))))
  name)

(define (description-extend! name description)
  (set! desc.init (cons (cons name description) desc.init))
  name)

(define (get-description name)
  (let ((p (assq name desc.init)))
    (and (pair? p) (cdr p))))

(define (global-fetch i)
  (vector-ref sg.current i))
(define (global-update! i v)
  (vector-set! sg.current i v))
(define (predefined-fetch i)
  (vector-ref sg.init i))

(define (get-dynamic-variable-index n)
  (let ((where (memq n *dynamic-variables*)))
    (if where
        (length where)
        (begin
          (set! *dynamic-variables* (cons n *dynamic-variables*))
          (length *dynamic-variables*)))))


(define (save-stack)
  (let ((copy (make-vector *stack-index*)))
    (vector-copy! *stack* copy 0 *stack-index*)
    copy))

(define (restore-stack copy)
  (set! *stack-index* (vector-length copy))
  (vector-copy! copy *stack* 0 *stack-index*))

(define original-vector-copy! vector-copy!)
(define (vector-copy! old new start end)
  (original-vector-copy! new start old start (- end start)))

; ============================================================================

(define (write-result-file ofilename comments dynamics
                           global-names constants code entry)
  (call-with-output-file ofilename
    (lambda (out)
      (for-each (lambda (comment) (display comment out))
                comments) (newline out)
      (display ";;; Dynamic variables" out) (newline out)
      (write dynamics out) (newline out) (newline out)
      (display ";;; Global modifiable variables" out) (newline out)
      (write global-names out) (newline out) (newline out)
      (display ";;; Quotations" out) (newline out)
      (write constants out) (newline out) (newline out)
      (display ";;; Bytecode" out) (newline out)
      (write code out) (newline out) (newline out)
      (display ";;; Entry point" out) (newline out)
      (write entry out) (newline out))))

; ============================================================================
; These implementations are not defined in the book, so I'm making them up...

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
  (caddr frame))

(define (activation-frame-next frame)
  (car frame))

; ============================================================================

(define undefined-value '*undefined*)

(g.current-initialize! 'x)
(g.current-initialize! 'y)
(g.current-initialize! 'z)
(g.current-initialize! 'foo)
(g.current-initialize! 'bar)
