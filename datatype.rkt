#lang eopl

(provide (all-defined-out))

(define-datatype
  program
  program?
  (a-program (a-program-exp expression?)))

(define-datatype
  expression expression?
  (const-exp (const number?))
  (bool-exp (bool boolean?))
  (var-exp (var symbol?))
  (if-exp
   (if-exp1 expression?)
   (if-exp2 expression?)
   (if-exp3 expression?))
  (lambda-exp
   (var symbol?)
   (body expression?)))
