(define-library (scheme base)
    (export call-with-current-continuation call/cc case cond dynamic-wind member memq memv)
    (import (sunny core)
            (sunny conditionals)
            (sunny dynwind)
            (sunny lists))
    (begin
      (define call-with-current-continuation call/cc)))
