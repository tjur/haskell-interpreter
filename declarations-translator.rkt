#lang eopl

(require (only-in racket/base foldr filter))
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
          (translate-to-let-without-body declarations)
          others)))))

(define matching-arguments?
  (lambda (arg1 arg2)
    (cases expression arg2
      (var-exp (var2)
        #t)
      (else
        (equal? arg1 arg2)))))

(define-datatype argument argument?
  (an-argument
    (pattern-exps (list-of expression?))
    (arguments (list-of (list-of argument?))))
  (ending-argument
    (pattern-exps (list-of expression?))
    (bodies (list-of expression?))))

(define var-exp?
  (lambda (exp)
    (cases expression exp
      (var-exp (var) #t)
      (else #f))))

(define get-same-and-rest
  (lambda (args args-bodies)
    (let* ([matching-lambda (lambda (arg-body) (matching-arguments? (car args) (caar arg-body)))]
           [matched (filter matching-lambda args-bodies)]
           [not-matched (filter (lambda (x) (not (matching-lambda x))) args-bodies)]
           [vars-pattern-lambda (lambda (arg-body) (var-exp? (caar arg-body)))]
           [matched-var-patterns (filter vars-pattern-lambda matched)])
      (list
        matched
        matched-var-patterns
        not-matched))))

(define group-arguments
  (lambda (args-bodies)
    (if (null? args-bodies) '()
      (let* ([arguments (caar args-bodies)]
             [result (get-same-and-rest arguments args-bodies)]
             [matched-other-patterns (list-ref result 0)]
             [matched-var-patterns (list-ref result 1)]
             [not-matched (list-ref result 2)]
             [matches (filter (lambda (xs) (not (null? xs))) (list matched-other-patterns matched-var-patterns))])
        (cons
          (if (null? (cdr arguments))
            (ending-argument
              (map caaar matches)
              (map cdar matches))
            (an-argument
              (map caaar matches)
              (map (lambda (arg-body-list)
                    (group-arguments (map (lambda (arg-body)
                                    (cons (cdar arg-body) (cdr arg-body))) arg-body-list))) matches)))

          (group-arguments not-matched))))))
                
(define get-pattern-var
  (lambda (pattern-var-n)
    (string->symbol (string-append "_$" (number->string pattern-var-n) "_"))))

(define get-match-exp
  (lambda (pattern-var pattern-exps then-bodies else-body)
    (if (null? pattern-exps) else-body
      (let* ([pattern-exp (car pattern-exps)]
             [then-body (car then-bodies)]
             [simple-match (lambda ()
                            (if-exp
                              (call-exp
                                (call-exp (var-exp '==) (var-exp pattern-var))
                                pattern-exp)
                              then-body
                              (get-match-exp pattern-var (cdr pattern-exps) (cdr then-bodies) else-body)))])
        (cases expression pattern-exp
          (const-num-exp (n)
            (simple-match))

          (const-bool-exp (b)
            (simple-match))

          (unit-exp ()
            (simple-match))

          (list-exp (xs)
            (if (null? xs)
              (if-exp
                (call-exp (var-exp 'empty) (var-exp pattern-var))
                then-body
                (get-match-exp pattern-var (cdr pattern-exps) (cdr then-bodies) else-body))

              (eopl:error "get-match-exp list-exp not null..."))) ;; TODO

          ;; TODO: lists & Datatypes

          (unpack-exp (var args)
            (if (eq? var ':)
              (let ([next-else-body (get-match-exp pattern-var (cdr pattern-exps) (cdr then-bodies) else-body)])
                (if-exp
                  (call-exp (var-exp 'empty) (var-exp pattern-var))
                  next-else-body

                  (let* ([head-arg (car args)]
                        [tail-arg (cadr args)]
                        [head-var (string->symbol (string-append (symbol->string pattern-var) "hd_"))]
                        [tail-var (string->symbol (string-append (symbol->string pattern-var) "tl_"))])

                    (let-exp
                      (list head-var)
                      (list (any-type))
                      (list (call-exp (var-exp 'head) (var-exp pattern-var)))
                      (get-match-exp head-var (list head-arg)
                        
                        (list
                          (let-exp
                            (list tail-var)
                            (list (any-type))
                            (list (call-exp (var-exp 'tail) (var-exp pattern-var)))
                            (get-match-exp tail-var (list tail-arg) (list then-body) next-else-body)))
                        next-else-body)))))

              (eopl:error "get-match-exp unpack-exp not implemented")))

          (var-exp (var)
            (let-exp (list var) (list (any-type)) (list (var-exp pattern-var))
              then-body))

          (else (eopl:error "impossible!")))))))

(define translate-to-lambdas
  (lambda (pattern-var pattern-var-n grouped-arguments-list)
    (if (null? grouped-arguments-list) (missing-case-exp)
      (let* ([arg (car grouped-arguments-list)]
             [next-pattern-var (get-pattern-var (+ pattern-var-n 1))]
             [next-case-body (translate-to-lambdas pattern-var pattern-var-n (cdr grouped-arguments-list))])
        (cases argument arg
          (an-argument (pattern-exps grouped-args-list)
            (let* ([next-bodies (map (lambda (grouped-args)
                                      (lambda-exp
                                        next-pattern-var
                                        (any-type)
                                        (translate-to-lambdas
                                          next-pattern-var
                                          (+ pattern-var-n 1)
                                          grouped-args))) grouped-args-list)])
              (get-match-exp pattern-var pattern-exps next-bodies next-case-body)))

          (ending-argument (pattern-exps bodies)
            (get-match-exp pattern-var pattern-exps bodies next-case-body)))))))

(define declaration-exp->var
  (lambda (declaration)
    (cases expression declaration
      (declaration-exp (var args body) var)
      (else (eopl:error "impossible!")))))

(define get-same-and-rest-declarations
  (lambda (var declarations)
    (let ([filter-lambda (lambda (declaration) (eq? var (declaration-exp->var declaration)))])
      (cons
        (filter filter-lambda declarations)
        (filter (lambda (x) (not (filter-lambda x))) declarations)))))

(define group-declarations-by-var
  (lambda (declarations)
    (if (null? declarations) '()
      (let ([declaration (car declarations)])
        (cases expression declaration
          (declaration-exp (var arguments body)
            (let* ([result (get-same-and-rest-declarations var declarations)]
                   [same (car result)]
                   [rest (cdr result)])
              (cons
                (cons var same)
                (group-declarations-by-var rest))))
          (else (eopl:error "THATS IMPOSSIBLE!")))))))

(define declaration-exp->argsnbody
  (lambda (declaration)
    (cases expression declaration
      (declaration-exp (var args body)
        (cons args body))
      (else (eopl:error "impossible!")))))

(define translate-to-let-without-body
  (lambda (declarations)
    (let* ([declarations-grouped (group-declarations-by-var declarations)]
           [groups (map (lambda (group)
                          (let* ([var (car group)]
                                 [declarations (cdr group)]
                                 [args-body-list (map declaration-exp->argsnbody declarations)])
                            (list
                              var
                              (any-type)
                              (start-translations args-body-list))))
                        declarations-grouped)]
           [joined (join3 groups)])
      (no-body-let
        (list-ref joined 0)
        (list-ref joined 1)
        (list-ref joined 2)))))

(define start-translations
  (lambda (args-body-list)
    (if (null? (caar args-body-list))
      (cdar args-body-list)
      (lambda-exp
        (get-pattern-var 0)
        (any-type)
        (translate-to-lambdas
          (get-pattern-var 0)
          0
          (group-arguments args-body-list))))))

(define join3
  (lambda (lst)
    (if (null? lst) (list '() '() '())
      (let* ([result (join3 (cdr lst))]
             [hd (car lst)])
        (list
          (cons (list-ref hd 0) (list-ref result 0))
          (cons (list-ref hd 1) (list-ref result 1))
          (cons (list-ref hd 2) (list-ref result 2)))))))
