#lang eopl

(require racket/match)
(require "datatypes.rkt")

(provide get-basic-procedure eval-arith-op)

(define arith-ops-list
  '(+ - * / ==))

(define get-arith-op-procedure
  (lambda (op)
    (proc-val
      (procedure
        '(x)
        (lambda-exp
          '(y)
          (arith-op-exp op (var-exp 'x) (var-exp 'y)))
        (empty-env)))))

(define eval-arith-op
  (lambda (op val1 val2)
    (match op
      ['+ (num-val (+ val1 val2))]
      ['- (num-val (- val1 val2))]
      ['* (num-val (* val1 val2))]
      ['/ (num-val (/ val1 val2))]
      ['== (bool-val (= val1 val2))])))

(define basic-procedures-list
  (map (lambda (op)
          (cons
            op
            (get-arith-op-procedure op)))
        arith-ops-list))

(define get-basic-procedure
  (lambda (var)
    (letrec ([aux (lambda (procedures)
                   (cond 
                    [(null? procedures) (eopl:error 'apply-env "No binding for ~s" var)]
                    [(eq? var (caar procedures)) (cdar procedures)]
                    [else (aux (cdr procedures))]))])
      (aux basic-procedures-list))))
