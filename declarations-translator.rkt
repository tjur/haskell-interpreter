#lang eopl

(require "datatypes.rkt")
(require "parser.rkt")

(provide translate-declarations let-without-body->let-exp)

(define-datatype let-without-body let-without-body?
  (no-body-let
   (p-names (list-of symbol?))
   (p-result-types (list-of type?))
   (exps (list-of expression?)))
  (empty))

(define let-without-body->let-exp
  (lambda (lwb body)
    (cases let-without-body lwb
      (no-body-let (p-names p-result-types exps)
        (let-exp p-names p-result-types exps body))
      (empty () body))))

;; translate-declarations : List(Exp) -> Let-without-body x List(Exp)
(define translate-declarations
  (lambda (exps)
    (letrec ([divide (lambda (exps)
                      (if (null? exps) (cons '() '())
                        (let* ([exp (car exps)]
                               [result (divide (cdr exps))]
                               [declarations (car result)]
                               [others (cdr result)])
                          (cases expression exp
                            (declaration-exp (var arguments body)
                              (cons
                                (cons exp declarations)
                                others))
                            
                            (else
                              (cons
                                declarations
                                (cons exp others)))))))])
      (let* ([result (divide exps)]
             [declarations (car result)]
             [others (cdr result)])
        (cons
          (translate-to-let declarations)
          others)))))

;; translate-to-let : List(Declaration-exp) -> Let-without-body
(define translate-to-let
  (lambda (declarations)
    (letrec ([split (lambda (declarations)
                      (if (null? declarations) (list '() '() '())
                        (cases expression (car declarations)
                          (declaration-exp (var arguments body)
                            (cons3
                              (list
                                (list var (any-type))
                                (map (lambda (x)
                                      (list (var-exp->symbol x) (any-type)))
                                  arguments)
                                body)
                              (split (cdr declarations))))
                          (else (eopl:error "THATS IMPOSSIBLE!")))))])
      (let ([result (split declarations)])
        (if (null? result) (empty)
          (let ([result-exp (create-let-exp (split declarations) (unit-exp))])
            (cases expression result-exp
              (let-exp (p-names p-result-types exps body)
                (no-body-let p-names p-result-types exps))
              (eopl:error "THATS IMPOSSIBLE!"))))))))

(define var-exp->symbol
  (lambda (exp)
    (cases expression exp
      (var-exp (var) var)
      (else (eopl:error "Not implemented yet!")))))
