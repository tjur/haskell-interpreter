#lang eopl

(require "datatypes.rkt")
(require "store.rkt")

(provide init-env empty-env extend-env extend-env* apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;
  
;; init-env : () -> Env 
(define init-env 
  (lambda ()
    (empty-env)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;


(define extend-env* 
  (lambda (bvars bvals saved-env)
    (if (null? bvars)
      saved-env
      (extend-env (car bvars) (car bvals)
        (extend-env* (cdr bvars) (cdr bvals) saved-env)))))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (cond 
                         ((location search-sym p-names)
                          => (lambda (n)
                               (newref
                                (proc-val
                                 (procedure 
                                  (list-ref b-vars n)
                                  (list-ref p-bodies n)
                                  env)))))
                         (else (apply-env saved-env search-sym)))))))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f
;; if sym is not in syms.
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))
