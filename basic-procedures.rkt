#lang eopl

(require racket/match)
(require (only-in racket/base foldl))
(require "environments.rkt")
(require "datatypes.rkt")
(require "store.rkt")

(provide init-basic-procedures get-basic-procedure eval-arith-op)

(define arith-ops-list
  '(+ - * / ==))

(define get-arith-op-procedure
  (lambda (op)
    (newref
      (proc-val
        (procedure
          '(x)
          (lambda-exp
            '(y)
            (arith-op-exp op (var-exp 'x) (var-exp 'y)))
          (empty-env))))))

(define eval-arith-op
  (lambda (op val1 val2)
    (match op
      ['+ (num-val (+ val1 val2))]
      ['- (num-val (- val1 val2))]
      ['* (num-val (* val1 val2))]
      ['/ (num-val (/ val1 val2))]
      ['== (bool-val (= val1 val2))])))

(define basic-procedures-list "uninitialized!")

(define get-basic-procedure
  (lambda (var)
    (cond
      ((apply-env basic-procedures-list var)
        => (lambda (ref1)
            (deref ref1)))
      (else (eopl:error 'apply-env "No binding for ~s" var)))))

(define init-basic-procedures
  (lambda ()
    (set! basic-procedures-list
      (foldl (lambda (op env)
              (extend-env op (get-arith-op-procedure op) env))
            (empty-env)
            arith-ops-list))))
