#lang eopl

(require racket/match)
(require (only-in racket/base foldl))
(require "environments.rkt")
(require "datatypes.rkt")
(require "store.rkt")

(provide (all-defined-out))

(define number-ops-list
  '(+ - * / < <= > >= mod))

(define create-number-procedure
  (lambda (op)
    (newref
      (proc-val
        (procedure
          'x
          (lambda-exp
            'y
            (int-type)
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
      ['< (bool-val (< val1 val2))]
      ['<= (bool-val (<= val1 val2))]
      ['> (bool-val (> val1 val2))]
      ['>= (bool-val (>= val1 val2))]
      ['mod (num-val (modulo val1 val2))])))

(define number-procedure-type-int-int-int
  (proc-type (int-type) (proc-type (int-type) (int-type))))

(define number-procedure-type-int-int-bool
  (proc-type (int-type) (proc-type (int-type) (bool-type))))

(define number-procedure-types
  (list
    (cons '+ number-procedure-type-int-int-int)
    (cons '- number-procedure-type-int-int-int)
    (cons '* number-procedure-type-int-int-int)
    (cons '/ number-procedure-type-int-int-int)
    (cons '< number-procedure-type-int-int-bool)
    (cons '<= number-procedure-type-int-int-bool)
    (cons '> number-procedure-type-int-int-bool)
    (cons '>= number-procedure-type-int-int-bool)
    (cons 'mod number-procedure-type-int-int-int)))

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
      ['head 
        (if (null? val)
          (eopl:error 'interpreter-error "head expects non-empty list\n")
          (car val))]
      ['tail
        (if (null? val)
          (eopl:error 'interpreter-error "tail expects non-empty list\n")
          (cadr val))]
      ['empty (newref (bool-val (null? val)))])))

(define list-procedure-types
  (list
    (cons 'head (proc-type (int-list-type) (int-type)))
    (cons 'tail (proc-type (int-list-type) (int-list-type)))
    (cons 'empty (proc-type (int-list-type) (bool-type)))))

(define common-operators
  (lambda ()
    (list
      (cons '==
        (newref
              (proc-val
                (procedure
                  'a
                  (lambda-exp
                    'b
                    (any-type)
                    (common-op-exp '== (var-exp 'a) (var-exp 'b)))
                  (empty-env)))))

      (cons '!=
        (newref
              (proc-val
                (procedure
                  'a
                  (lambda-exp
                    'b
                    (any-type)
                    (common-op-exp '!= (var-exp 'a) (var-exp 'b)))
                  (empty-env))))))))

(define eval-common-operator
  (lambda (op val1 val2)
    (match op
      ['== (bool-val (equal? val1 val2))]
      ['!= (bool-val (not (equal? val1 val2)))])))

(define common-operators-types
  (list
    (cons '== (proc-type (any-type) (proc-type (any-type) (bool-type))))
    (cons '!= (proc-type (any-type) (proc-type (any-type) (bool-type))))))

(define basic-procedures-list
  (lambda ()
    (append
      (number-procedures)
      (list-procedures)
      (common-operators))))

(define basic-procedures-types
  (append
    number-procedure-types
    list-procedure-types
    common-operators-types))

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
