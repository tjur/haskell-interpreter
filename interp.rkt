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
                  filter))
(require (only-in racket/string
                  string-join))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

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
                          [others (cdr translated)]
                          [lets
                            (map (lambda (exp)
                                  (let-without-body->let-exp let-without-body exp))
                                  others)])
                    ;;;  (display (pretty-print-exp (car lets)))))))))
                     (evaluate-expressions lets tenv env)))))))


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
                 [ty (type-of (car exps) tenv)]
                 [val (value-of/k (car exps) env (end-cont))])
            (begin
              (display (pretty-print-exp-result val ty i))
              (if (null? (cdr exps)) ;; last expression
                  42
                  (eval-exp-aux (cdr exps) (+ i 1))))))))

  (eval-exp-aux exps 1))


(define pretty-print-val-constr
  (lambda (val)
    (cases val-constr-exp val
      (a-val-constr (name types)
        (string-join (map symbol->string (cons name types)) " ")))))


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

      (type-value-exp (id val-constr-name b-vars ty)
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

      (missing-case-exp ()
        (eopl:error "Missing declaration case!\n"))

      (check-data-exp-val-exp (exp1 val-constr-name)
                          (value-of/k exp1 env
                                      (check-data-exp-val-cont val-constr-name cont)))

      (extract-from-data-exp-val-exp (exp1 index)
                                     (value-of/k exp1 env
                                                 (extract-from-data-exp-val-cont index cont)))

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

(define run
  (lambda (string)
    (run-program (scan&parse string))))

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


#|
(run "\\ (x :: int) -> (x + 1) 5")

(run-type "42")
(run-type "True")
(run-type "\\(x :: int) -> (x + 1)")
;;; (run-type "if True then 2 else False")
(run-type "\\ (xs :: list) (ys :: list) -> ((head xs) + (head ys))")

(run-type "let (f :: int) (x :: unit) = 100 (g :: int) (x :: int) (y :: int) (z :: list) = x - y in (g 43 1 [])")

(run-type
 "let (f :: int) (x :: int) (y :: unit) (z :: bool) (lst :: list) = 42 in f")

(run-type
 "let (even :: bool) (x :: int) = if x == 0 then True else (odd (x - 1))
      (odd :: bool) (x :: int) = if x == 0 then False else (even(x - 1))
      in (even 42)")

(run
 "let (even :: bool) (x :: int) = if x == 0 then True else (odd (x - 1))
      (odd :: bool) (x :: int) = if x == 0 then False else (even(x - 1))
      in (even 42)")

(run-type
 "let (fact :: int) (n :: int) = if n == 0 then 1 else (n * (fact (n - 1))) in (fact 5)")

(run
 "let (fact :: int) (n :: int) = if n == 0 then 1 else (n * (fact (n - 1))) in (fact 5)")
|#

;;; (run "data Tree = Empty | Leaf int | Node Tree int Tree;
;;;              data Bin = Zero | One;

;;;              let (f :: Tree) (x :: Tree) = (Leaf y)
;;;                  (y :: int) = 26 + 1 in
;;;               (f (Node (Leaf 42) 27 (Node (Leaf 42) 27 (Leaf 1))));

;;;              if 2 == 3 then Zero else One;

;;;              let (lst :: int-list) = [1+2, 4*5, 100 - 1] in ((head lst) : (tail lst));

;;;              let (take :: int-list) (lst :: int-list) (n :: int) =
;;;               if n == 0
;;;                  then []
;;;                  else ((head lst) : (take (tail lst) (n - 1)))
;;;              in let (lst :: int-list) (n :: int) = (n : (lst (n + 1)))
;;;               in let (nats :: int-list) = (lst 0)
;;;                in (take nats 50)")

;;; (run "(fact :: int) (0 :: int) = 1;
;;;       (fact :: int) (n :: int) = n * (fact (n - 1));
;;;       fact 5")

;;; (run "(and :: bool) (True :: bool) (True :: bool) = True;
;;;       (and :: bool) (x :: bool) (y :: bool) = False;
      
;;;       True `and` False")

;;; (run "(sum :: int) ([] :: int-list) = 0;
;;;       (sum :: int) (x:xs :: int-list) = x + (sum  xs);

;;;       sum [1, 2, 3, 4]")

;;; (run "(rev :: int-list) ([] :: int-list) (acc :: int-list) = acc;
;;;       (rev :: int-list) (x:xs :: int-list) (acc :: int-list) = rev xs (x:acc);

;;;       rev [5, 4, 3, 2, 1] []")

;;; (run "(len :: int) ([] :: int-list) = 0;
;;;       (len :: int) (x:(y:xs) :: int-list) = 2 + (len xs);
;;;       (len :: int) (x:xs :: int-list) = 1 + (len xs);

;;;       len [1, 2, 3, 4, 5, 10]")

;;; (run "(fib :: int) (0 :: int) = 0;
;;;       (fib :: int) (1 :: int) = 1;
;;;       (fib :: int) (n :: int) = (fib (n - 1)) + (fib (n - 2));

;;;       (ones :: int-list) = 1:ones;
      
;;;       (head (tail ones)) + (head ones)")

(run "data Tree = Leaf | Node Tree int Tree;

      (sumTree :: int) (Leaf :: Tree) = 0;
      (sumTree :: int) (Node l x r :: Tree) = sumTree l + x + (sumTree r);

      sumTree (Node Leaf 10 (Node Leaf 5 Leaf))")
