#lang eopl

(require "parser.rkt")
(require "datatypes.rkt")
(require "environments.rkt")
(require "store.rkt")
(require "pretty-printer.rkt")
(require "basic-procedures.rkt")
(require "type-checker.rkt")
(require "declarations-translator.rkt")
(require "data-expression.rkt")
(require (only-in racket/base
                  filter void))
(require (only-in racket/string
                  string-join))

(provide run run-test)


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define env-with-declarations 'empty)

;; run : string -> ()
(define run
  (lambda (string)
    (begin
      (initialize-types-to-check-list!)
      (run-program (scan&parse string)))))

;; run-program : Program -> ()
(define run-program 
  (lambda (pgm)
    (initialize-store!)
    (init-basic-procedures)
    (cases program pgm
      (a-program (global-exps)
                 (let ([expressions
                        (filter (lambda (global-exp) (expression? global-exp)) global-exps)]
                       [data-exps
                        (filter (lambda (global-exp) (data-exp? global-exp)) global-exps)])

                   (let* ([env (process-data-exps data-exps (init-env))]
                          [tenv (create-tenv (init-tenv) env)]
                          [translated (translate-declarations expressions)]
                          [let-without-body (car translated)]
                          [empty-let (let-without-body->let-exp let-without-body (empty-exp))]
                          [other-exps (cdr translated)])
                    (begin
                      (type-of empty-let tenv)
                      (value-of/k empty-let env (end-cont))
                      (evaluate-expressions other-exps tenv-with-declarations env-with-declarations))))))))


(define (create-tenv tenv env)
  (define create-tenv-aux
    (lambda (tenv env)
      (cases environment env
        (empty-env () tenv)
        (extend-env (val-constr-name ref saved-env)
                    (let* ([exp
                            (cases thunk (deref ref)
                              (a-thunk (exp _) exp))]
                           [val-constr-type (type-of-exp exp)])
                      (create-tenv-aux
                       (extend-tenv val-constr-name val-constr-type tenv)
                       saved-env)))
        (else (eopl:error 'create-tenv "Expected empty-env or extend-env but found: ~s" env)))))
  
  (create-tenv-aux tenv env))


(define (evaluate-expressions exps tenv env)
  (define eval-exp-aux
    (lambda (exps i)
      (if (null? exps)
          (display "No expression to evaluate!\n")
          (let* ([exp (car exps)]
                 [ty (type-of exp tenv)]
                 [val (value-of/k exp env (end-cont))])
            (begin
              (display (pretty-print-exp-result val ty i))
              (if (null? (cdr exps)) ;; last expression
                  (void)
                  (eval-exp-aux (cdr exps) (+ i 1))))))))

  (eval-exp-aux exps 1))


(define (run-test string)
    
  (define run-test-program
    (lambda (pgm)
      (initialize-store!)
      (init-basic-procedures)
      (cases program pgm
        (a-program (global-exps)
                  (let ([expressions
                          (filter (lambda (global-exp) (expression? global-exp)) global-exps)]
                        [data-exps
                          (filter (lambda (global-exp) (data-exp? global-exp)) global-exps)])

                   (let* ([env (process-data-exps data-exps (init-env))]
                          [tenv (create-tenv (init-tenv) env)]
                          [translated (translate-declarations expressions)]
                          [let-without-body (car translated)]
                          [empty-let (let-without-body->let-exp let-without-body (empty-exp))]
                          [other-exps (cdr translated)]
                          [exp (car other-exps)])
                    (begin
                      (type-of empty-let tenv)
                      (value-of/k empty-let env (end-cont))
                      (type-of exp tenv-with-declarations)
                      (pretty-print-expval
                        (value-of/k exp env-with-declarations (end-cont))))))))))

  (begin
    (initialize-types-to-check-list!)
    (run-test-program (scan&parse string))))


;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-num-exp (num) (apply-cont cont (num-val num)))
      
      (const-bool-exp (bool) (apply-cont cont (bool-val bool)))

      (unit-exp () (apply-cont cont (unit-val)))
      
      (var-exp (var)
               (cond
                ((apply-env env var)
                  => (lambda (ref1)
                      (let ((w (deref ref1)))
                        (if (expval? w)
                            (apply-cont cont w)
                            (value-of-thunk/k w (thunk-cont ref1 cont))))))
                (else (apply-cont cont (get-basic-procedure var)))))

      (list-exp (exps)
                (value-of-list/k exps env cont))

      (type-value-exp (val-constr-name b-vars b-vars-types ty)
                      (let ([values (map (lambda (var) (apply-env env var)) b-vars)])
                        (apply-cont cont (data-exp-val val-constr-name values ty))))

      (cons-exp (exp1 exp2)
                (apply-cont cont
                  (list-val
                    (list
                      (newref (a-thunk exp1 env))
                      (newref (a-thunk exp2 env))))))
      
      (lambda-exp (vars types body)
                (apply-cont cont 
                            (proc-val (procedure vars body env))))
      
      (let-exp (p-names p-result-types ps-vars ps-vars-types p-bodies let-body)
                  (value-of/k let-body
                              (make-extend-env-rec p-names p-bodies env)
                              cont))
      
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-cont exp2 exp3 env cont)))
      
      (call-exp (rator rands)
                (value-of/k rator env
                            (rator-cont rands env cont)))

      (number-op-exp (op exp1 exp2)
                      (value-of/k exp1 env
                                  (number-op-cont1 op exp2 env cont)))

      (list-proc-exp (proc exp1)
                      (value-of/k exp1 env
                                  (list-proc-cont proc cont)))

      (common-op-exp (op exp1 exp2)
                      (value-of/k exp1 env
                                  (common-op-cont1 op exp2 env cont)))

      (missing-case-exp (var)
        (eopl:error 'interpreter-error (string-append "Missing declaration case for " (symbol->string var) "\n")))

      (check-data-exp-val-exp (exp1 val-constr-name)
                          (value-of/k exp1 env
                                      (check-data-exp-val-cont val-constr-name cont)))

      (extract-from-data-exp-val-exp (exp1 index)
                                     (value-of/k exp1 env
                                                 (extract-from-data-exp-val-cont index cont)))

      (empty-exp ()
                  (set! env-with-declarations env)
                  (apply-cont cont (unit-val)))

      (else (eopl:error "Not implemented for ~s" exp))
      
      )))


;; apply-cont : Cont * ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (cases expval val
                  (list-val (refs)
                            (begin
                                  (map eval-thunk-in-ref refs) ;; refs values are changed
                                  val))
                  (data-exp-val (val-constr-name refs type)
                                (begin
                                  (map eval-thunk-in-ref refs) ;; refs values are changed
                                  val))
                  (else val)))
                               
      
      (if-cont (exp2 exp3 saved-env saved-cont)
                    (if (expval->bool val)
                        (value-of/k exp2 saved-env saved-cont)
                        (value-of/k exp3 saved-env saved-cont)))
      
      (cons1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2
                              saved-env (cons2-cont val saved-cont)))
      
      (cons2-cont (val1 saved-cont)
                  (let ((tail (expval->list val)))
                    (apply-cont saved-cont
                                (list-val (cons val1 tail)))))

      (tail-cont (tail-exps saved-env saved-cont)
                 (value-of-list/k tail-exps saved-env
                             (head-cont val saved-cont)))

      (head-cont (head-val saved-cont)
                 (apply-cont saved-cont (list-val (list head-val (newref val)))))
      
      (rator-cont (rand saved-env saved-cont)
                  (let ((proc (expval->proc val)))
                    (apply-procedure/k
                      proc
                      (newref (a-thunk rand saved-env))
                      saved-cont)))

      (thunk-cont (ref1 saved-cont)
                  (begin
                    (setref! ref1 val)   
                    (apply-cont saved-cont val)))

      (number-op-cont1 (op exp2 saved-env saved-cont)
                      (value-of/k exp2 saved-env
                                  (number-op-cont2 op val saved-cont)))

      (number-op-cont2 (op val1 saved-cont)
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val)))
                          (apply-cont saved-cont
                                      (eval-number-procedure op num1 num2))))

      (list-proc-cont (op saved-cont)
                        (let* ((xs (expval->list val))
                               (ref1 (eval-list-procedure op xs))
                               (val1 (deref ref1)))
                          (if (expval? val1)
                            (apply-cont saved-cont val1)
                            (value-of-thunk/k val1 (thunk-cont ref1 saved-cont)))))

      (common-op-cont1 (op exp2 saved-env saved-cont)
                        (value-of/k exp2 saved-env
                                    (common-op-cont2 op val saved-cont)))

      (common-op-cont2 (op val1 saved-cont)
                        (apply-cont saved-cont
                                    (eval-common-operator op val1 val)))

      (check-data-exp-val-cont (val-constr-name-to-compare saved-cont)
                               (cases expval val
                                 (data-exp-val (val-constr-name values type)
                                               (apply-cont saved-cont
                                                           (bool-val (equal?
                                                                      val-constr-name
                                                                      val-constr-name-to-compare))))
                                 (else (eopl:error 'apply-cont
                                                   "Expected data-exp-val but got: ~s" val))))

      (extract-from-data-exp-val-cont (index saved-cont)
                                      (cases expval val
                                        (data-exp-val (val-constr-name values type)
                                                      (let* ([ref (list-ref values index)]
                                                             [w (deref ref)])
                                                        (if (expval? w)
                                                            (apply-cont saved-cont w)
                                                            (value-of-thunk/k w (thunk-cont ref saved-cont)))))
                                        (else (eopl:error 'apply-cont
                                                          "Expected data-exp-val but got: ~s" val))))
      
      )))


;; apply-procedure/k : Proc * ExpVals * Cont -> FinalAnswer
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
        (apply-cont
          (tail-cont (cdr exps) env cont)
          (newref
            (a-thunk (car exps) env))))))


;; value-of-thunk : Thunk * Cont -> FinalAnswer
(define value-of-thunk/k
  (lambda (th cont)
    (cases thunk th
      (a-thunk (exp1 saved-env)
               (value-of/k exp1 saved-env cont)))))


;; eval-thunk-in-ref : Ref -> Expval (evaluates thunk in ref)
(define eval-thunk-in-ref
  (lambda (ref)
    (let ((w (deref ref)))
      (if (expval? w)
          (apply-cont (end-cont) w)
          (value-of-thunk/k w (thunk-cont ref (end-cont)))))))
