(set! foo (lambda (a)
            (* a (dynamic b))))

(dynamic-let (b 10)
  (cons '(result "value") (foo 5)))
