
(define-library (libs book)

  (export a-true-value
          atom?
          call-with-input-file
          call-with-output-file
          display
          empty-begin
          extend
          invoke
          lookup
          make-vector
          newline
          the-false-value
          update!
          vector-append
          write
          wrong)

  (import (builtin core)
          (libs utils))

  (begin
    (define wrong error)
    (define empty-begin 'empty-begin)
    (define the-false-value (cons "boolean" "false"))
    (define a-true-value (cons "boolean" "true"))

    (define (atom? exp) (not (pair? exp)))

    (define (lookup id env)
      (if (pair? env)
          (if (eq? (caar env) id)
              (cdar env)
              (lookup id (cdr env)))
          (wrong "No such binding" id)))

    (define (update! id env value)
      (if (pair? env)
          (if (eq? (caar env) id)
              (begin (set-cdr! (car env) value)
                     value)
              (update! id (cdr env) value))
          (wrong "No such binding" id)))

    (define (extend env variables values)
      (cond ((pair? variables)
             (if (pair? values)
                 (cons (cons (car variables) (car values))
                       (extend env (cdr variables) (cdr values)))
                 (wrong "Too few values")))
            ((null? variables)
             (if (null? values)
                 env
                 (wrong "Too many values")))
            ((symbol? variables) (cons (cons variables values) env))))

    (define (invoke fn args)
      (if (procedure? fn)
          (fn args)
          (wrong "Not a function" fn)))

    (define builtin-display display)
    (define builtin-newline newline)

    (define (display obj . target)
      (if (null? target)
          (builtin-display obj)
          (fdisplay (car target) obj)))

    (define (newline . target)
      (if (null? target)
          (builtin-newline)
          (fdisplay (car target) "\n")))

    (define (write obj f)
      (fwrite f obj))

    (define (call-with-input-file filename func)
      (let* ((file (file-open filename 'r))
             (result (func file)))
        (file-close! file)
        result))

    (define (call-with-output-file filename func)
      (let* ((file (file-open filename 'w))
             (result (func file)))
        (file-close! file)
        result))

    (define builtin-make-vector make-vector)

    (define (make-vector size . fill)
      (let ((vec (builtin-make-vector size)))
        (if (pair? fill)
            (fill-vector! vec (car fill)))
        vec))

    (define (fill-vector! vec fill)
      (define (scan n m)
        (if (< n m)
            (begin (vector-set! vec n fill)
                   (scan (+ n 1) m))))
      (scan 0 (vector-length vec)))

    (define (vector-append v1 v2)
      (let* ((n1 (vector-length v1))
             (n2 (vector-length v2))
             (new-vec (make-vector (+ n1 n2))))
        (vector-copy! new-vec 0 v1 0 n1)
        (vector-copy! new-vec n1 v2 0 n2)
        new-vec))))
