(define-library (scheme base)
    (export caar cadr cdar cddr
            call-with-current-continuation call/cc
            current-error-port current-input-port current-output-port
            case cond
            display
            dynamic-wind
            make-parameter
            member memq memv
            newline
            parameterize)
    (import (sunny core)
            (sunny conditionals)
            (sunny dynwind)
            (sunny lists)
            (sunny parameter)
            (sunny ports))
    (begin
      (define call-with-current-continuation call/cc)))
