(set! bar (cons 1 2))
(set! foo set-car!)
(foo bar 42)
(set! foo set-cdr!)
(foo bar 666)
bar

