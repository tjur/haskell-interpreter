#lang eopl

(require "store.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;; program and expression ;;;;;;;;;;;;;;;;

(define-datatype program program?
  (a-program (a-program-exp (list-of expression?))))

(define-datatype expression expression?
  (const-num-exp (const number?))
  (const-bool-exp (bool boolean?))
  (unit-exp)
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
   (rands (list-of expression?)))
  (let-exp
   (vars (list-of symbol?))
   (args (list-of (list-of symbol?)))
   (exps (list-of expression?))
   (body expression?))
  (cons-exp
   (head expression?)
   (tail expression?))
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


;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (unit-val)
  (list-val
   (list list?))
  (proc-val 
   (proc proc?)))

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

(define expval->unit
  (lambda (v)
    (cases expval v
      (unit-val () (an-unit))
      (else (expval-extractor-error 'unit v)))))

;; extracts any value
(define expval->val
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (bool-val (bool) bool)
      (proc-val (proc) proc)
      (list-val (lst) lst)
      (unit-val () (an-unit)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvars (list-of symbol?))
   (body expression?)
   (env environment?)))


;;;;;;;;;;;;;;;; unit ;;;;;;;;;;;;;;;;

(define-datatype unit unit?
  (an-unit))


;;;;;;;;;;;;;;;; environment ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval reference?)               
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of (list-of symbol?)))
   (proc-bodies (list-of (lambda (x) (or (expression? x) (reference? x)))))
   (saved-env environment?)))


;;;;;;;;;;;;;;;; thunk ;;;;;;;;;;;;;;;;

;; a-thunk : Exp * Env -> Thunk
(define-datatype thunk thunk?
  (a-thunk
   (exp1 expression?)
   (env environment?)))


;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  (end-cont)                 
  (if-cont 
   (exp2 expression?)
   (exp3 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (cons1-cont                
   (tail expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (cons2-cont                
   (head-val expval?)
   (saved-cont continuation?))
  (head-cont
   (head-val expval?)
   (saved-cont continuation?))
  (tail-cont
   (tail-exps (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (rator-cont            
   (rand expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (rand-cont             
   (proc-val expval?)
   (saved-cont continuation?))
  (thunk-cont
   (ref reference?)
   (saved-cont continuation?)))
