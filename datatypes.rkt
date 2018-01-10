#lang eopl

(provide (all-defined-out))

(define-datatype
  program
  program?
  (a-program (a-program-exp (list-of expression?))))

(define-datatype
  expression
  expression?
  (const-exp (const number?))
  (bool-exp (bool boolean?))
  (undefined-exp)
  (var-exp (var symbol?))
  (list-exp
    (list (list-of (lambda (_) #t))))
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
    (vars (list-of symbol?))
    (args (list-of (list-of symbol?)))
    (exps (list-of expression?))
    (body expression?))
  (op-exp
    (op symbol?)
    (exp1 expression?)
    (exp2 expression?))
  (cons-exp
    (head expression?)
    (tail expression?))
  (head-exp
    (exp expression?))
  (tail-exp
    (exp expression?))
  (data-exp
    (type-constr symbol?)
    (val-constrs (list-of val-constr-exp?)))
  (unpack-exp
    (val-constr symbol?)
    (values (list-of expression?)))
  (declaration-exp
    (var symbol?)
    (arguments (list-of expression?))
    (body expression?))
  (op-declaration-exp
    (op symbol?)
    (arg1 expression?)
    (arg2 expression?)
    (body expression?)))

(define-datatype
  val-constr-exp
  val-constr-exp?
  (val-constr
    (name symbol?)
    (types (list-of symbol?))))
