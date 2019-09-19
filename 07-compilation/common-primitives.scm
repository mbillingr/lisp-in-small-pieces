
(define (defprimitive name value arity)
  (case arity
    ((0) (defprimitive0 name value))
    ((1) (defprimitive1 name value))
    ((2) (defprimitive2 name value))
    ((3) (defprimitive3 name value))
    (else static-wrong "Unsupported primitive arity" name arity)))

(define (defprimitive0 name value)
  (definitial name
    (let* ((arity+1 (+ 0 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value)
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value))
      (make-closure behavior sr.init))))

(define (defprimitive1 name value)
  (definitial name
    (let* ((arity+1 (+ 1 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value (activation-frame-argument v* 0))
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value a))
      (make-closure behavior sr.init))))

(define (defprimitive2 name value)
  (definitial name
    (let* ((arity+1 (+ 2 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value (activation-frame-argument v* 0)
                          (activation-frame-argument v* 1))
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value a b))
      (make-closure behavior sr.init))))

(define (defprimitive3 name value)
  (definitial name
    (let* ((arity+1 (+ 3 1))
           (behavior
             (lambda (v* sr)
               (if (= arity+1 (activation-frame-argument-length v*))
                   (value (activation-frame-argument v* 0)
                          (activation-frame-argument v* 1)
                          (activation-frame-argument v* 2))
                   (wrong "Incorrect arity" name)))))
      (description-extend! name `(function ,value a b c))
      (make-closure behavior sr.init))))

(definitial 't #t)
(definitial 'f #f)
(definitial 'nil '())
(defprimitive 'null? null? 1)
(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defprimitive 'eq? eq? 2)
(defprimitive '= = 2)
(defprimitive '< < 2)
(defprimitive '<= <= 2)
(defprimitive '> > 2)
(defprimitive '>= >= 2)
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defprimitive '* * 2)
(defprimitive '/ / 2)
