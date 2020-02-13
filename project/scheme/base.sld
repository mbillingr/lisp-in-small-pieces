(define-library (scheme base)
    (export call-with-current-continuation call/cc case cond dynamic-wind member memq memv)
    (import (except (sunny core) call/cc)
            (sunny conditionals)
            (sunny dynwind)
            (sunny lists))
    (begin
      (define call/cc call-with-current-continuation)))
