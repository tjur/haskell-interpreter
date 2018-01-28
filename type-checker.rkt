#lang eopl

(require "datatypes.rkt")
(require "basic-procedures.rkt")
(require "pretty-printer.rkt")
(require (only-in racket/base
                  foldr void))

(provide type-of-exp type-of type-to-external-form init-tenv extend-tenv tenv-with-declarations)

(define tenv-with-declarations 'empty)

(define any-type?
  (lambda (ty)
    (cases type ty
      (any-type () #t)
      (else #f))))

;; check-equal-type! : Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (and
            (and (not (any-type? ty1)) (not (any-type? ty2)))
            (not (equal? ty1 ty2)))
      (report-unequal-types ty1 ty2 exp))))

(define check-many-types!
  (lambda (types1 types2 exps)
    (if (null? types1)
        (void)
        (begin
          (check-equal-type! (car types1) (car types2) (car exps))
          (check-many-types! (cdr types1) (cdr types2) (cdr exps))))))

;; report-unequal-types : Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'type-error  
                "Types didn't match: ~s != ~a in~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                (pretty-print-exp exp))))

;; type-to-external-form : Type -> List | Sym
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (unit-type () '())
      (int-list-type () 'int-list)
      (proc-type (arg-type result-type)
                 (list
                  (type-to-external-form arg-type)
                  '->
                  (type-to-external-form result-type)))
      (data-exp-type (name) name)
      
      (else (eopl:error 'type-to-external-form "Not implemented for type: ~s" ty))
      
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

      (list-exp (lst)
                (begin
                  (map (lambda (exp1) (check-equal-type! (type-of exp1 tenv) (int-type) exp1)) lst)
                  (int-list-type)))

      (type-value-exp (val-constr-name b-vars b-vars-types type) type)

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

      (cons-exp (head tail)
                (let ((ty1 (type-of head tenv))
                      (ty2 (type-of tail tenv)))
                  (check-equal-type! ty1 (int-type) head)
                  (check-equal-type! ty2 (int-list-type) tail)
                  (int-list-type)))
      
      (let-exp (p-names p-result-types ps-vars ps-vars-types p-bodies letrec-body)
               (let* ([p-types
                       (map (lambda (p-result-type p-vars-types)
                              (foldr proc-type p-result-type p-vars-types)) p-result-types ps-vars-types)]
                        
                      [tenv-for-letrec-body
                       (extend-tenv2* p-names p-types tenv)])

                 (begin
                   (let ([truncated-p-bodies (map (lambda (p-body p-vars-types) (truncate p-body (length p-vars-types))) p-bodies ps-vars-types)])
                     (check-many-types!
                      p-result-types
                      (map
                       (lambda (p-body p-vars p-vars-types) (type-of p-body (extend-tenv2* p-vars p-vars-types tenv-for-letrec-body)))
                       truncated-p-bodies ps-vars ps-vars-types)
                      truncated-p-bodies))
                      
                   (type-of letrec-body tenv-for-letrec-body))))

      (missing-case-exp (var) (any-type))

      (check-data-exp-val-exp (exp val-constr-name) (bool-type))

      (extract-from-data-exp-val-exp (exp index) (any-type))

      (empty-exp ()
                    (set! tenv-with-declarations tenv)
                    (unit-type))

      (else (eopl:error 'type-error
             "Type checker is not defined for expression: ~s" exp))

      )))


;; removes added earlier vars to expression (as a lambda-exp) to have proper type of expression
(define truncate
  (lambda (exp i)
    (if (zero? i)
        exp
        (cases expression exp
          (lambda-exp (var ty body)
                      (truncate body (- i 1)))
          (else (eopl:error
                 "Expected lambda-exp but got: ~s" exp))))))


(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (eopl:error 'type-error
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

(define extend-tenv2*
  (lambda (vars types tenv)
    (if (null? vars) tenv
      (extend-tenv2* (cdr vars) (cdr types)
        (extend-tenv (car vars) (car types) tenv)))))

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
                  (empty-tenv))))
