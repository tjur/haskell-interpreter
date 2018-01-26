#lang eopl

(require "parser.rkt")
(require "datatypes.rkt")
(require "environments.rkt")
(require "store.rkt")
(require "pretty-printer.rkt")
(require "basic-procedures.rkt")
(require "type-checker.rkt")
(require "declarations-translator.rkt")


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> FinalAnswer
(define values-of-program 
  (lambda (pgm)
    (initialize-store!)
    (init-basic-procedures)
    (cases program pgm
      (a-program (exps)
                 (let* ([result (translate-declarations exps)]
                        [let-without-body (car result)]
                        [others (cdr result)])
                  (map (lambda (exp)
                        (value-of/k
                          (let-without-body->let-exp let-without-body exp)
                          (init-env)
                          (end-cont)))
                    others))))))

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

      (cons-exp (exp1 exp2)
                (apply-cont cont
                  (list-val
                    (list
                      (newref (a-thunk exp1 env))
                      (newref (a-thunk exp2 env))))))
      
      (lambda-exp (vars types body)
                (apply-cont cont 
                            (proc-val (procedure vars body env))))
      
      (let-exp (p-names p-result-types p-bodies let-body)
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

      (else (eopl:error "Not implemented for ~s" exp))
      
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



(define run
  (lambda (string)
    (map (lambda (val)
          (display 
            (pretty-print-expval val))
          (newline))
      (values-of-program (scan&parse string)))
    (newline)))

(define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1) (type-of-exp (car exp1))))))

(define run-type
  (lambda (string)
    (display
      (type-to-external-form
        (type-of-program (scan&parse string))))
    (newline)))

;;; (run "27")

;;; (run "True")

;;; (run "()")

;;; (run "[1, 2, 3, 4, 5]")

;;; (run "if True then 1 else 0")

;;; (run "if False then 1 else 0")

;;; (run "let x = 42 in x")

;;; (run "0:(1:(2:[]))")

;;; (run "let xs = 1:(2:[]) in (head xs)")

;;; (run "let add1 = (+) 1 in add1 10")

;;; (run "let f a = if a then a else a in f ((==) 1 2)")

#|
(run "let ones = 1:ones in (head (tail ones))")
(run "empty (tail (tail [1, 2]))")
(run "empty (tail (tail (1:(2:[]))))")

(run "let a = 1:b b = 2:a in (head a + (head (tail a)))")

(run "1 + 2 + 3 + 4 + 5 - 5")

(run "0 == 1")

(run "5 - 2 * 2") ;; łączność w lewo, brak priorytetu :-(  :'(    <- ja też płaczę

(run "let add x y = x + y in (add 2)")

(run "let o f g = \\x -> (f (g x))
      in
        (head `o` tail `o` tail [1, 2, 3])")
|#

;;; (run "\\ (x :: int) -> (x + 1) 5")

;;; (run "let (f :: int) (x :: int) (y :: int) = x + y in (f 1 9)")

;;; (run "let (f :: bool) (x :: unit) = 100 (g :: int) (x :: int) (y :: int) = x - y in (g 43 1)")

;;; (run-type "42")
;;; (run-type "True")
;;; (run-type "\\(x :: int) -> (x + 1)")
;;; ;;; (run-type "if True then 2 else False")
;;; (run-type "\\ (xs :: list) (ys :: list) -> ((head xs) + (head ys))")

;;; (run "(.) f g = \\ (x :: any) -> (f (g x));

;;;       (++) xs ys = if empty xs
;;;                    then ys
;;;                    else ((head xs):(tail xs ++ ys));

;;;       fact x = if x == 0
;;;                then 1
;;;                else (x * (fact (x - 1)));

;;;       head . tail . tail ([1] ++ [2, 3]);
;;;       fact 5")

;;; (display
;;;   (cases program (scan&parse "f x = x; f 5")
;;;     (a-program (exps)
;;;       (translate-declarations exps))))

(run "fact 0 = 1;
      fact n = n * (fact (n - 1));
      fact 5")

(run "and True True = True;
      and x y = False;
      
      True `and` False")
