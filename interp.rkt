#lang eopl

(require "parser.rkt")
(require "datatypes.rkt")
(require "environments.rkt")
(require "store.rkt")


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> FinalAnswer
(define value-of-program 
  (lambda (pgm)
    (initialize-store!) 
    (cases program pgm
      (a-program (exp1)
                 (value-of/k (car exp1) (init-env) (end-cont))))))


;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-num-exp (num) (apply-cont cont (num-val num)))
      
      (const-bool-exp (bool) (apply-cont cont (bool-val bool)))

      (unit-exp () (apply-cont cont (unit-val)))
      
      (var-exp (var)
               (let [[ref1 (apply-env env var)]]
                        (let ((w (deref ref1)))
                          (if (expval? w)
                              (apply-cont cont w)
                              (value-of-thunk/k w (thunk-cont ref1 cont))))))

      (list-exp (exps)
                (value-of-list/k exps env cont))

      (cons-exp (exp1 exp2)
                (value-of/k exp1 env
                            (cons1-cont exp2 env cont)))
      
      (lambda-exp (vars body)
                (apply-cont cont 
                            (proc-val (procedure vars body env))))
      
      (let-exp (p-names b-vars p-bodies let-body)
                  (value-of/k let-body
                              (extend-env-rec* p-names b-vars p-bodies env)
                              cont))
      
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-cont exp2 exp3 env cont)))
      
      (call-exp (rator rand) 
                (value-of/k rator env
                            (rator-cont rand env cont)))

      (else (eopl:error "Not implemented"))
      
      )))


;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val)
      
      (if-cont (exp2 exp3 saved-env saved-cont)
                    (if (expval->bool val)
                        (value-of/k exp2 saved-env saved-cont)
                        (value-of/k exp3 saved-env saved-cont)))
      
      (cons1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2
                              saved-env (cons2-cont val saved-cont)))
      
      (cons2-cont (val1 saved-cont)
                  (let ((head (expval->val val1))
                        (tail (expval->list val)))
                    (apply-cont saved-cont
                                (list-val (cons head tail)))))

      (tail-cont (tail-exps saved-env saved-cont)
                 (value-of-list/k tail-exps saved-env
                             (head-cont val saved-cont)))

      (head-cont (head-val saved-cont)
                 (apply-cont saved-cont (list-val (cons head-val (expval->list val)))))
      
      (rator-cont (rand saved-env saved-cont)
                  (value-of/k rand saved-env
                              (rand-cont val saved-cont)))
      
      (rand-cont (val1 saved-cont)
                 (let ((proc (expval->proc val1)))
                   (apply-procedure/k proc val saved-cont)))

      (thunk-cont (ref1 saved-cont)
                  (begin
                    (setref! ref1 val)   
                    (apply-cont saved-cont val)))
                  
      )))


;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var arg saved-env)
                             cont)))))


(define value-of-list/k
  (lambda (exps env cont)
    (if (null? exps)
        (apply-cont cont (list-val '()))
        (value-of/k (car exps) env
                    (tail-cont (cdr exps) env cont)))))


;; value-of-thunk : Thunk * Cont -> FinalAnswer
(define value-of-thunk/k
  (lambda (th cont)
    (cases thunk th
      (a-thunk (exp1 saved-env)
               (value-of/k exp1 saved-env cont)))))




(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run "27")

(run "True")

(run "()")

(run "[1, 2, 3, 4, 5]")

(run "if True then 1 else 0")

(run "if False then 1 else 0")

(run "1:(2:[])")
