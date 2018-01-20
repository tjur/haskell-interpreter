#lang eopl

(require racket/match)
(require (only-in racket/base foldl))
(require "environments.rkt")
(require "datatypes.rkt")
(require "store.rkt")

(provide init-basic-procedures get-basic-procedure eval-number-procedure eval-list-procedure)

(define number-ops-list
  '(+ - * / ==))

(define create-number-procedure
  (lambda (op)
    (newref
      (proc-val
        (procedure
          'x
          (lambda-exp
            'y
            (number-op-exp op (var-exp 'x) (var-exp 'y)))
          (empty-env))))))

(define number-procedures
  (lambda ()
    (map
      (lambda (op)
        (cons op (create-number-procedure op)))
      number-ops-list)))

(define eval-number-procedure
  (lambda (op val1 val2)
    (match op
      ['+ (num-val (+ val1 val2))]
      ['- (num-val (- val1 val2))]
      ['* (num-val (* val1 val2))]
      ['/ (num-val (/ val1 val2))]
      ['== (bool-val (= val1 val2))])))

(define create-list-procedure
  (lambda (proc)
    (newref (proc-val
              (procedure 'xs
                (list-proc-exp proc (var-exp 'xs))
              (empty-env))))))

(define list-procedures
  (lambda ()
    (map
      (lambda (proc)
        (cons proc (create-list-procedure proc)))
      (list 'head 'tail 'empty))))

(define eval-list-procedure
  (lambda (proc val)
    (match proc
      ['head (car val)]
      ['tail (cadr val)]  ;; niby listy trzymamy jako pary, ale tak naprawdę to są to listy (dwu/zero)elementowe
      ['empty (newref (bool-val (null? val)))]))) ;; TODO: ten newref jest trochę przykry, ale interp czeka na referencję

(define basic-procedures-list
  (lambda ()
    (append
      (number-procedures)
      (list-procedures))))

(define basic-procedures-env "uninitialized!")

(define get-basic-procedure
  (lambda (var)
    (cond
      ((apply-env basic-procedures-env var)
        => (lambda (ref1)
            (deref ref1)))
      (else (eopl:error 'apply-env "No binding for ~s" var)))))

(define init-basic-procedures
  (lambda ()
    (set! basic-procedures-env
      (foldl (lambda (element env)
              (let ([var (car element)]
                    [proc (cdr element)])
                (extend-env var proc env)))
            (empty-env)
            (basic-procedures-list)))))