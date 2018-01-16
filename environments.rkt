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
                               (let [(vars (list-ref b-vars n))
                                     (body (list-ref p-bodies n))]
                                 (if (null? vars) ;; empty vars means this is some variable not a function
                                     body ;; body is here a reference created earlier
                                     (newref
                                      (proc-val
                                       (procedure 
                                        vars
                                        body
                                        env)))))))
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

;; for variables (empty (car b-vars)) creates newref to thunk
(define (make-extend-env-rec p-names b-vars p-bodies env)
  
  (define create-newrefs-for-vars-and-extend-env-rec
    (lambda (p-names b-vars p-bodies p-names-acc b-vars-acc p-bodies-acc var-refs)
      (if (null? p-names)
          (list (extend-env-rec* (reverse p-names-acc) (reverse b-vars-acc) (reverse p-bodies-acc) env) var-refs)
          (if (null? (car b-vars)) ;; empty (car b-vars) means it is a variable not a function
              (let* [(exp1 (car p-bodies))
                     (ref (newref (a-thunk exp1 (empty-env))))] ;; temporary env - we have to change it later
                (create-newrefs-for-vars-and-extend-env-rec
                 (cdr p-names) (cdr b-vars) (cdr p-bodies)
                 (cons (car p-names) p-names-acc) (cons '() b-vars-acc) (cons ref p-bodies-acc) (cons ref var-refs)))
              (create-newrefs-for-vars-and-extend-env-rec
               (cdr p-names) (cdr b-vars) (cdr p-bodies)
               (cons (car p-names) p-names-acc) (cons (car b-vars) b-vars-acc) (cons (car p-bodies) p-bodies-acc) var-refs)))))

  (define update-var-refs-with-new-env
    (lambda (var-refs new-env)
      (if (null? var-refs)
          new-env
          (let* [(ref (car var-refs))
                 (th (deref ref))]
            (cases thunk th
              (a-thunk (exp _)
                       (begin
                         (setref! ref (a-thunk exp new-env))
                         (update-var-refs-with-new-env (cdr var-refs) new-env))))))))

  (let* [(result (create-newrefs-for-vars-and-extend-env-rec
                  p-names b-vars p-bodies '() '() '() '()))
         (new-env (list-ref result 0))
         (var-refs (list-ref result 1))]
    (update-var-refs-with-new-env var-refs new-env)))
