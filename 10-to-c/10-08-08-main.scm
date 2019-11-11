
(define (generate-main out form)
  (format out "~%/* Expression: */~%void main(void) {~%")
  (format out "  SCM_print")
  (between-parentheses out
    (->C form out))
  (format out ";~%  exit(0);~%}~%"))
