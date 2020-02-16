(define-library (scheme base)
    (export caar cadr cdar cddr
            call-with-current-continuation call/cc
            case cond
            dynamic-wind
            make-parameter 
            member memq memv
            parameterize)
    (import (sunny core)
            (sunny conditionals)
            (sunny dynwind)
            (sunny lists)
            (sunny parameter))
    (begin
      (define call-with-current-continuation call/cc)))
