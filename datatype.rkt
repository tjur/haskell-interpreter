#lang eopl

(provide (all-defined-out))

(define-datatype
  program
  program?
  (a-program (a-program27 expression?)))

(define-datatype
  expression
  expression?
  (const-exp (const-exp28 number?))
  (var-exp (var-exp29 symbol?))
  (if-exp
    (if-exp30 expression?)
    (if-exp31 expression?)
    (if-exp32 expression?))
  (list-exp
    (list list-expression?)))

(define-datatype
  list-expression
  list-expression?
  (empty-list-exp)
  (cons-list-exp
    (head expression?)
    (tail list-expression?)))
