#lang eopl

(provide (all-defined-out))

(define-datatype
  program
  program?
  (a-program (a-program-exp expression?)))

(define-datatype
  expression
  expression?
  (const-exp (const number?))
  (bool-exp (bool boolean?))
  (undefined-exp)
  (var-exp (var symbol?))
  (if-exp
    (if-exp1 expression?)
    (if-exp2 expression?)
    (if-exp3 expression?))
  (lambda-exp
    (vars (list-of symbol?))
    (body expression?))
  (call-exp
   (rator expression?)
   (rand (list-of expression?)))
  (let-exp
    (var symbol?)
    (args (list-of symbol?))
    (exp expression?)
    (body expression?))
  (arith-exp
    (op symbol?)
    (exp1 expression?)
    (exp2 expression?))
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
