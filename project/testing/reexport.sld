(define-library (testing reexport)
    (export foo
            (rename cons kons))
    (import (testing lib)
            (sunny core)))
