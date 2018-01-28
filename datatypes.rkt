#lang eopl

(require "store.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;; program and expression ;;;;;;;;;;;;;;;;

(define-datatype program program?
  (a-program (a-program-exp (list-of (lambda (x) (or (expression? x) (data-exp? x)))))))

(define-datatype expression expression?
  (const-num-exp (const number?))
  (const-bool-exp (bool boolean?))
  (unit-exp)
  (var-exp (var symbol?))
  (list-exp
   (list (list-of expression?)))
  (type-value-exp ;; value which type is a type defined by some data-exp
   (val-constr-name symbol?) ;; name of value contructor that creates that value
   (b-vars (list-of symbol?)) ;; b-vars that takes value constr before it returns that value
   (b-vars-types (list-of type?))
   (type type?))
  (if-exp
   (if-exp1 expression?)
   (if-exp2 expression?)
   (if-exp3 expression?))
  (lambda-exp
   (var symbol?)
   (var-type type?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (let-exp
   (p-names (list-of symbol?))
   (p-result-types (list-of type?))
   (ps-vars (list-of (list-of symbol?))) ;; original vars that were later moved to the bodies of its procedures - need to store that for type-checking let
   (ps-vars-types (list-of (list-of type?))) ;; types of original vars - need to store that for type-checking let
   (changed-p-bodies (list-of expression?))
   (body expression?))
  (cons-exp
   (head expression?)
   (tail expression?))
  (unpack-exp
   (val-constr symbol?)
   (values (list-of expression?)))
  (declaration-exp
   (var (lambda (xs) (and (list? xs) (not (null? xs)) (symbol? (car xs)) (type? (cadr xs)))))
   (arguments (list-of (lambda (xs) (and (list? xs) (not (null? xs)) (expression? (car xs)) (type? (cadr xs))))))
   (body expression?))
  (number-op-exp
   (op symbol?)
   (exp1 expression?)
   (exp2 expression?))
  (list-proc-exp
   (proc symbol?)
   (exp1 expression?))
  (common-op-exp
   (op symbol?)
   (exp1 expression?)
   (exp2 expression?))
  (missing-case-exp
   (var symbol?))
  (check-data-exp-val-exp
   (expression-to-check expression?)
   (val-constr-name-to-compare symbol?))
  (extract-from-data-exp-val-exp
   (exp expression?)
   (index number?))
  (empty-exp)) ;; artificial expression, does not occur in program's ast
               ;; used for initializing env with global declarations


;;;;;;;;;;;;;;;; data expression ;;;;;;;;;;;;;;;;

(define-datatype data-exp data-exp?
  (a-data-exp
   (data-exp-type type?)
   (val-constrs (list-of val-constr-exp?))))

(define-datatype val-constr-exp val-constr-exp?
  (a-val-constr
   (name symbol?)
   (types (list-of type?))))


;;;;;;;;;;;;;;;; types ;;;;;;;;;;;;;;;;

(define-datatype type type?
  (any-type)
  (int-type)
  (bool-type)
  (unit-type)
  (int-list-type)
  (proc-type
   (var-type type?)
   (body-type type?))
  (data-exp-type
   (type-constr-name symbol?)))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (unit-val)
  (list-val
   (list (list-of reference?)))
  (proc-val 
   (proc proc?))
  (data-exp-val
   (val-constr-name symbol?)
   (values (list-of reference?))
   (type type?))) ;; type of the value

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

(define expval->data-exp
  (lambda (v)
    (cases expval v
      (data-exp-val (name values ty) values)
      (else (expval-extractor-error 'data-exp-val v)))))

;; extracts any value
(define expval->val
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (bool-val (bool) bool)
      (proc-val (proc) proc)
      (list-val (lst) lst)
      (unit-val () (an-unit))
      (data-exp-val (name values ty) values))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
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
   (head-val reference?)
   (saved-cont continuation?))
  (tail-cont
   (tail-exps (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (rator-cont            
   (rand expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (thunk-cont
   (ref reference?)
   (saved-cont continuation?))
  (number-op-cont1
   (op symbol?)
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (number-op-cont2
   (op symbol?)
   (val1 expval?)
   (saved-cont continuation?))
  (list-proc-cont
   (op symbol?)
   (saved-cont continuation?))
  (common-op-cont1
   (op symbol?)
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (common-op-cont2
   (op symbol?)
   (val1 expval?)
   (saved-cont continuation?))
  (check-data-exp-val-cont
   (val-constr-name symbol?)
   (saved-cont continuation?))
  (extract-from-data-exp-val-cont
   (index number?)
   (saved-cont continuation?)))
