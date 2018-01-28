#lang eopl

(require "datatypes.rkt")
(require "store.rkt")
(require (only-in racket/base
                  reverse))

(provide init-env empty-env extend-env apply-env extend-env* make-extend-env-rec)


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
      (empty-env () #f)
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-rec* (p-names p-bodies saved-env)
                       (cond 
                         ((location search-sym p-names)
                          => (lambda (n)
                               (let [(body (list-ref p-bodies n))]
                                 body))) ;; body is here a reference created earlier                                    
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


(define (make-extend-env-rec p-names p-bodies env)
  
  (define create-newrefs-and-extend-env-rec
    (lambda (p-names p-bodies p-names-acc p-bodies-acc var-refs)
      (if (null? p-names)
          (list (extend-env-rec* (reverse p-names-acc) (reverse p-bodies-acc) env) var-refs)
          (let* [(exp1 (car p-bodies))
                 (ref (newref (a-thunk exp1 (empty-env))))] ;; temporary env - we have to change it later with update-refs-with-new-env
            (create-newrefs-and-extend-env-rec
             (cdr p-names) (cdr p-bodies)
             (cons (car p-names) p-names-acc) (cons ref p-bodies-acc) (cons ref var-refs))))))

  (define update-refs-with-new-env
    (lambda (refs new-env)
      (if (null? refs)
          new-env
          (let* [(ref (car refs))
                 (th (deref ref))]
            (cases thunk th
              (a-thunk (exp _)
                       (begin
                         (setref! ref (a-thunk exp new-env))
                         (update-refs-with-new-env (cdr refs) new-env))))))))

  (let* [(result (create-newrefs-and-extend-env-rec
                  p-names p-bodies '() '() '()))
         (new-env (list-ref result 0))
         (refs (list-ref result 1))]
    (update-refs-with-new-env refs new-env)))
