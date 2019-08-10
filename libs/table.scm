
;;   this is a very crude implementation of a table
;;   using unordered association lists
(define-library (libs table)
  (export make-table insert lookup)

  (import (builtin core))

  (begin
    (define (make-record key value) (cons key value))
    (define (key record) (car record))
    (define (value record) (cdr record))

    (define (lookup given-key set-of-records)
      (cond ((null? set-of-records) #f)
            ((equal? given-key (key (car set-of-records)))
             (lambda () (value (car set-of-records))))
            (else (lookup given-key (cdr set-of-records)))))

    (define (adjoin-set x set)
      (if (element-of-set? x set)
          set
          (cons x set)))

    (define (insert given-key value set-of-records)
      (define (iter remaining-set result)
        (cond ((null? remaining-set)
               (cons (make-record given-key value)
                     result))
              ((equal? given-key (key (car remaining-set)))
               (append (cons (make-record given-key value)
                           result)
                     (cdr remaining-set)))
              (else (iter (cdr remaining-set)
                          (cons (car remaining-set)
                                result)))))
      (iter set-of-records '()))

    (define (append list1 list2)
      (if (null? list1)
          list2
          (cons (car list1)
                (append (cdr list1)
                        list2))))

    (define (make-table)
      '())))
