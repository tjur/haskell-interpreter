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
  (arith-exp
    (op symbol?)
    (exp1 expression?)
    (exp2 expression?))
  (if-exp
    (if-exp30 expression?)
    (if-exp31 expression?)
    (if-exp32 expression?))
  (list-exp
    (list list-expression?))
  (head-exp
    (exp expression?))
  (tail-exp
    (exp expression?))
  (cons-exp
    (exp1 expression?)
    (exp2 expression?))
  (append-exp
    (exp1 expression?)
    (exp2 expression?)))

(define-datatype
  list-expression
  list-expression?
  (empty-list-exp)
  (cons-list-exp
    (head expression?)
    (tail list-expression?)))
