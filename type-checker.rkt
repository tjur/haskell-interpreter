#lang eopl

(require "datatypes.rkt")
(require "basic-procedures.rkt")
(require "pretty-printer.rkt")

(provide type-of-exp type-to-external-form)


;; check-equal-type! : Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not
           (or
            (equal? ty1 ty2)
            (or
             (equal? ty1 (any-type))
             (equal? ty2 (any-type)))))
      (report-unequal-types ty1 ty2 exp))))

;; report-unequal-types : Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!  
                "Types didn't match: ~s != ~a in~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                (pretty-print-exp exp))))

;; type-to-external-form : Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (unit-type () '())
      (list-type () 'list)
      (any-type () 'any)
      (proc-type (arg-type result-type)
                 (list
                  (type-to-external-form arg-type)
                  '->
                  (type-to-external-form result-type)))
      (else 'not-implemented)
      
      )))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;
  
;; type-of-exp : Exp -> Type
(define type-of-exp
  (lambda (exp)
    (type-of exp (init-tenv))))

;; type-of : Exp * Tenv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp

      (const-num-exp (num) (int-type))

      (const-bool-exp (bool) (bool-type))

      (unit-exp () (unit-type))

      (list-exp (lst) (list-type))

      (var-exp (var) (apply-tenv tenv var))

      (if-exp (exp1 exp2 exp3)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv))
                    (ty3 (type-of exp3 tenv)))
                (check-equal-type! ty1 (bool-type) exp1)
                (check-equal-type! ty2 ty3 exp)
                ty2))

      (lambda-exp (var var-type body)
                (let ((result-type
                       (type-of body
                                (extend-tenv var var-type tenv))))
                  (proc-type var-type result-type)))

      (call-exp (rator rand) 
                (let ((rator-type (type-of rator tenv))
                      (rand-type  (type-of rand tenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else
                     (report-rator-not-a-proc-type rator-type rator)))))

      #|(number-op-exp (op exp1 exp2)
                     (let ((ty1 (type-of exp1 tenv))
                           (ty2 (type-of exp2 tenv)))
                       (check-equal-type! ty1 (int-type) exp1)
                       (check-equal-type! ty2 (int-type) exp2)
                       (result-type-number-procedure op)))|#

      #|(list-proc-exp (proc exp1)
                     (let ((ty1 (type-of exp1 tenv)))
                       (check-equal-type! ty1 (list-type) exp1)
                       (result-type-list-procedure proc)))|#

      (cons-exp (head tail)
                (let ((ty1 (type-of tail tenv)))
                  (check-equal-type! ty1 (list-type) tail)
                  (list-type)))

      ;; TODO
      #|
      (let-exp (p-names p-result-types p-bodies letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv p-name
                                      (proc-type b-var-type p-result-type)
                                      tenv)))
                    (let ((p-body-type 
                           (type-of p-body
                                    (extend-tenv b-var b-var-type
                                                 tenv-for-letrec-body)))) 
                      (check-equal-type!
                       p-body-type p-result-type p-body)
                      (type-of letrec-body tenv-for-letrec-body))))
      |#

      (missing-case-exp () (any-type))

      (else (eopl:error
             "Type checker is not defined for expression: ~s" exp))

      )))


(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-of-expression
                "Rator not a proc type:~%~s~%had rator type ~s"   
                rator 
                (type-to-external-form rator-type))))


;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
    
(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))
    
(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)
(define extend-tenv*
  (lambda (lst tenv)
    (if (null? lst) tenv
      (extend-tenv* (cdr lst)
        (extend-tenv (caar lst) (cdar lst) tenv)))))

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 old-env)
                            (if (eqv? sym sym1) 
                                val1
                                (apply-tenv old-env sym))))))
  
(define init-tenv
  (lambda ()
    (extend-tenv* basic-procedures-types
      (extend-tenv 'x (int-type) 
                  (extend-tenv 'v (int-type)
                                (extend-tenv 'i (int-type)
                                            (empty-tenv)))))))
