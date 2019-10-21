(import (builtin core)
        (libs utils)
        (libs book))

(include "common-stuff.scm")
(include "instructions.scm")
(include "primitives.scm")

(define *constants* 'anything)
(define *code* 'anything)
(define *pc* 'anything)
(define *env* 'anything)
(define *stack* 'anything)
(define *stack-index* 'anything)
(define *val* 'anything)
(define *fun* 'anything)
(define *arg1* 'anything)
(define *arg2* 'anything)
(define *exit* 'anything)

(define (run-application stack-size filename)
  (let* ((content (file-read filename))
         (dynamics      (car content))
         (global-names  (cadr content))
         (constants     (caddr content))
         (code          (cadddr content))
         (entry-points  (car (cddddr content))))
    (set! sg.current.names    global-names)
    (set! *dynamic-variables* dynamics)
    (set! sg.current          (make-vector (length sg.current.names)
                                           undefined-value))
    (set! *constants*         constants)
    (set! *code*              (vector))
    (install-code! code)
    (set! *env*               sr.init)
    (set! *stack*             (make-vector stack-size))
    (set! *stack-index*       0)
    (set! *val*               'anything)
    (set! *fun*               'anything)
    (set! *arg1*              'anything)
    (set! *arg2*              'anything)
    (push-dynamic-binding
      0 (list (make-primitive (lambda ()
                                (show-exception)
                                ;(*exit* 'aborted)))))
                                (set! *exit* 'aborted)))))
    (stack-push 1)    ; pc for FINISH
    (if (pair? entry-points)
        (for-each stack-push entry-points)
        (stack-push entry-points))
    (set! *pc* (stack-pop))
    ;(call/cc (lambda (exit)
    ;           (set! *exit* exit)
    ;           (run)))
    (set! *exit* #f)
    (run)))

; ===========================================================================

(define (invoke f tail?)
  (cond ((closure? f) (if (not tail?)
                          (stack-push *pc*))
                      (set! *env* (closure-closed-environment f))
                      (set! *pc* (closure-code f)))
        ((primitive? f) (if (not tail?)
                            (stack-push *pc*))
                        ((primitive-address f)))
        ((continuation? f) (continuation-invoke f tail?))
        ((escape? f) (escape-invoke f tail?))
        (else (signal-exception #f (list "Not a function" f)))))

(define (make-closure code closed-environment)
  (vector 'closure code closed-environment))

(define (closure? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'closure)))

(define (closure-code obj)
  (vector-ref obj 1))

(define (closure-closed-environment obj)
  (vector-ref obj 2))

(define (make-continuation stack)
  `(continuation ,stack))

(define (continuation? obj)
  (and (pair? obj) (eq? (car obj) 'continuation)))

(define (continuation-stack obj)
  (cadr obj))

(define (continuation-invoke f tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (begin
        (restore-stack (continuation-stack f))
        (set! *val* (activation-frame-argument *val* 0))
        (set! *pc* (stack-pop)))
      (signal-exception #f (list "Incorrect arity" 'continuation))))

(define (make-escape stack-index)
  `(escape ,stack-index))

(define (escape? obj)
  (and (pair? obj) (eq? (car obj) 'escape)))

(define (escape-stack-index obj)
  (cadr obj))

(define (escape-invoke f tail?)
  (if (= (+ 1 1) (activation-frame-argument-length *val*))
      (if (escape-valid? f)
          (begin (set! *stack-index* (escape-stack-index f))
                 (set! *val* (activation-frame-argument *val* 0))
                 (set! *pc* (stack-pop)))
          (signal-exception #f (list "Escape out of extent" f)))
      (signal-exception #f (list "Incorrect arity" 'escape))))

(define (escape-valid? f)
  (let ((index (escape-stack-index f)))
    (and (>= *stack-index* index)
         (eq? f (vector-ref *stack* (- index 3)))
         (eq? escape-tag (vector-ref *stack* (- index 2))))))

; ===========================================================================

(define (show-exception)
  (println "Unhandled exception:" *val*))

; ===========================================================================

; ===========================================================================

(println (run-application 1000 "08-reflection/reflective-interpreter.sco"))
