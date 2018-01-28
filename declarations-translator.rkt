#lang eopl

(require (only-in racket/base
                  foldl
                  foldr
                  filter
                  build-list
                  length
                  void
                  format))

(require "datatypes.rkt")
(require "parser.rkt")
(require "data-expression.rkt")
(require "pretty-printer.rkt")
(require "type-checker.rkt")

(provide translate-declarations let-without-body->let-exp)

(define-datatype let-without-body let-without-body?
  (no-body-let
   (p-names (list-of symbol?))
   (p-result-types (list-of type?))
   (ps-vars (list-of (list-of symbol?)))
   (ps-vars-types (list-of (list-of type?)))
   (exps (list-of expression?)))
  (empty))

(define let-without-body->let-exp
  (lambda (lwb body)
    (cases let-without-body lwb
      (no-body-let (p-names p-result-types ps-vars ps-vars-types exps)
        (let-exp p-names p-result-types ps-vars ps-vars-types exps body))
      (empty () body))))

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
    (pattern-exps (list-of (lambda (xs) (and (list? xs) (not (null? xs)) (expression? (car xs)) (type? (cadr xs))))))
    (arguments (list-of (list-of argument?))))
  (ending-argument
    (pattern-exps (list-of (lambda (xs) (and (list? xs) (not (null? xs)) (expression? (car xs)) (type? (cadr xs))))))
    (bodies (list-of expression?))))

(define var-exp?
  (lambda (exp)
    (cases expression exp
      (var-exp (var) #t)
      (else #f))))

(define get-same-and-rest
  (lambda (args args-bodies)
    (let* ([matching-lambda (lambda (arg-body) (matching-arguments? (caar args) (caaar arg-body)))]
           [matched (filter matching-lambda args-bodies)]
           [not-matched (filter (lambda (x) (not (matching-lambda x))) args-bodies)]
           [vars-pattern-lambda (lambda (arg-body) (and (not (var-exp? (caar args))) (var-exp? (caaar arg-body))))]
           [matched-var-patterns (filter vars-pattern-lambda matched)]
           [exact-match-lambda (lambda (arg-body) (equal? (caar args) (caaar arg-body)))]
           [exact-matched (filter exact-match-lambda args-bodies)]
           [exact-not-matched (filter (lambda (x) (not (exact-match-lambda x))) args-bodies)])
      (list
        matched
        matched-var-patterns
        not-matched
        exact-matched
        exact-not-matched))))

(define group-arguments
  (lambda (args-bodies)
    (if (null? args-bodies) '()
      (let* ([arguments (caar args-bodies)]
             [result (get-same-and-rest arguments args-bodies)]
             [matched-other-patterns (list-ref result 0)]
             [matched-var-patterns (list-ref result 1)]
             [not-matched (list-ref result 2)]
             [exact-matched (list-ref result 3)]
             [exact-not-matched (list-ref result 4)]
             [matches (filter (lambda (xs) (not (null? xs))) (list matched-other-patterns matched-var-patterns))])
        (if (null? (cdr arguments))
          (cons
            (ending-argument
              (list (caaar exact-matched))
              (list (cdar exact-matched)))
            (group-arguments exact-not-matched))

          (cons
            (an-argument
              (map caaar matches)
              (map (lambda (arg-body-list)
                    (group-arguments (map (lambda (arg-body)
                                            (cons (cdar arg-body) (cdr arg-body))) arg-body-list))) matches))

            (group-arguments not-matched)))))))
                
(define get-pattern-var
  (lambda (pattern-var-n)
    (string->symbol (string-append "_$" (number->string pattern-var-n)))))

(define get-match-exp
  (lambda (declaration-var pattern-var pattern-exps then-bodies else-body)
    (if (null? pattern-exps) else-body
      (let* ([pattern-exp (caar pattern-exps)]
             [pattern-type (cadar pattern-exps)]
             [then-body (car then-bodies)]
             [simple-match (lambda ()
                            (if-exp
                              (call-exp
                                (call-exp (var-exp '==) (var-exp pattern-var))
                                pattern-exp)
                              then-body
                              (get-match-exp declaration-var pattern-var (cdr pattern-exps) (cdr then-bodies) else-body)))])
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
                (get-match-exp declaration-var pattern-var (cdr pattern-exps) (cdr then-bodies) else-body))

            (get-match-exp declaration-var pattern-var ;; translating to unpack-exp ':
              (cons
                (list
                  (unpack-exp ': (list (car xs) (list-exp (cdr xs))))
                  (int-list-type))
                (cdr pattern-exps))
              then-bodies
              else-body)))

          (unpack-exp (var args)
            (let ([next-else-body (get-match-exp declaration-var pattern-var (cdr pattern-exps) (cdr then-bodies) else-body)])
              (if (eq? var ':)
                (if-exp
                  (call-exp (var-exp 'empty) (var-exp pattern-var))
                  next-else-body

                  (let* ([head-arg (car args)]
                        [tail-arg (cadr args)]
                        [head-var (string->symbol (string-append (symbol->string pattern-var) "hd"))]
                        [tail-var (string->symbol (string-append (symbol->string pattern-var) "tl"))])

                    (let-exp
                      (list head-var)
                      (list (int-type))
                      (list '())
                      (list '())
                      (list (call-exp (var-exp 'head) (var-exp pattern-var)))
                      (get-match-exp declaration-var head-var (list (list head-arg (int-type)))
                        
                        (list
                          (let-exp
                            (list tail-var)
                            (list (int-list-type))
                            (list '())
                            (list '())
                            (list (call-exp (var-exp 'tail) (var-exp pattern-var)))
                            (get-match-exp declaration-var tail-var (list (list tail-arg (int-list-type))) (list then-body) next-else-body)))
                        next-else-body))))

                (let* ([val-constr-arg-types (get-val-constr-arg-types var)]
                       [val-constr-arg-length (length val-constr-arg-types)]
                       [args-length (length args)])

                  (if (< args-length val-constr-arg-length)
                    (eopl:error 'syntax-error
                      (string-append "Too few arguments in " (symbol->string declaration-var) " declaration in " (pretty-print-exp pattern-exp) "\n"))
                    (if (> args-length val-constr-arg-length)
                      (eopl:error 'syntax-error
                        (string-append "Too many arguments in " (symbol->string declaration-var) " declaration in " (pretty-print-exp pattern-exp) "\n"))

                      (if-exp
                        (check-data-exp-val-exp (var-exp pattern-var) var)

                          (foldr
                            (lambda (i arg then-body)
                              (let ([new-var (string->symbol (string-append (symbol->string pattern-var) "[" (number->string i) "]"))]
                                    [new-var-type (list-ref (get-val-constr-arg-types var) i)])
                                (let-exp
                                  (list new-var)
                                  (list new-var-type)
                                  (list '())
                                  (list '())
                                  (list (extract-from-data-exp-val-exp (var-exp pattern-var) i))
                                  (get-match-exp
                                    declaration-var
                                    new-var
                                    (list (list (list-ref args i) new-var-type))
                                    (list then-body)
                                    next-else-body))))
                            then-body
                            (build-list (length args) values)
                            args)

                          next-else-body)))))))

          (var-exp (var)
            (let-exp
              (list var)
              (list pattern-type)
              (list '())
              (list '())
              (list (var-exp pattern-var))
              then-body))

          (else (eopl:error "impossible!")))))))

(define translate-bodies
  (lambda (declaration-var pattern-var pattern-var-n grouped-arguments-list)
    (if (null? grouped-arguments-list) (missing-case-exp declaration-var)
      (let* ([arg (car grouped-arguments-list)]
             [next-pattern-var (get-pattern-var (+ pattern-var-n 1))]
             [next-case-body (translate-bodies declaration-var pattern-var pattern-var-n (cdr grouped-arguments-list))])
        (cases argument arg
          (an-argument (pattern-exps grouped-args-list)
            (let* ([next-bodies (map (lambda (grouped-args)
                                        (translate-bodies
                                          declaration-var
                                          next-pattern-var
                                          (+ pattern-var-n 1)
                                          grouped-args)) grouped-args-list)])
              (get-match-exp declaration-var pattern-var pattern-exps next-bodies next-case-body)))

          (ending-argument (pattern-exps bodies)
            (get-match-exp declaration-var pattern-var pattern-exps bodies next-case-body)))))))

(define declaration-exp->var
  (lambda (declaration)
    (car (declaration-exp->var-type declaration))))

(define declaration-exp->var-type
  (lambda (declaration)
    (cases expression declaration
      (declaration-exp (var-type args body) var-type)
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
          (declaration-exp (var-type arguments body)
            (let* ([result (get-same-and-rest-declarations (car var-type) declarations)]
                   [same (car result)]
                   [rest (cdr result)])
              (cons
                (cons var-type same)
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
                          (let* ([var-and-type (car group)]
                                 [var-name (car var-and-type)]
                                 [var-type (cadr var-and-type)]
                                 [declarations (cdr group)]
                                 [args-body-list (map declaration-exp->argsnbody declarations)])
                            (begin
                              (check-declaration-types! var-name declarations)
                              (list
                                var-name
                                var-type
                                (build-list (length (caar args-body-list)) get-pattern-var)
                                (map cadr (caar args-body-list))
                                (start-translations var-name args-body-list)))))
                        declarations-grouped)]
           [joined (join5 groups)])
      (no-body-let
        (list-ref joined 0)
        (list-ref joined 1)
        (list-ref joined 2)
        (list-ref joined 3)
        (list-ref joined 4)))))

(define start-translations
  (lambda (declaration-var args-body-list)
    (let ([first-args (caar args-body-list)])
      (if (null? first-args)
        (cdar args-body-list)
        (begin
          (check-declaration-arguments-types! declaration-var args-body-list)
          (lambdaize-arguments
            first-args
            0
            (translate-bodies
              declaration-var
              (get-pattern-var 0)
              0
              (group-arguments args-body-list))))))))

(define join5
  (lambda (lst)
    (if (null? lst) (list '() '() '() '() '())
      (let* ([result (join5 (cdr lst))]
             [hd (car lst)])
        (list
          (cons (list-ref hd 0) (list-ref result 0))
          (cons (list-ref hd 1) (list-ref result 1))
          (cons (list-ref hd 2) (list-ref result 2))
          (cons (list-ref hd 3) (list-ref result 3))
          (cons (list-ref hd 4) (list-ref result 4)))))))


(define lambdaize-arguments
  (lambda (args var-i body)
    (if (null? args)
      body
      (let* ([arg-type (car args)]
             [type (cadr arg-type)])
        (lambda-exp
          (get-pattern-var var-i)
          type
          (lambdaize-arguments (cdr args) (+ var-i 1) body))))))

(define check-declaration-arguments-types!
  (lambda (declaration-var args-body-list)
    (let* ([first-args (caar args-body-list)]
           [types (map cadr first-args)])
      (for-each
        (lambda (args-body)
          (for-each
            (lambda (arg type)
              (let ([arg-type (cadr arg)])
                (check-if-types-match! declaration-var type arg-type)))
            (car args-body)
            types))
        args-body-list))))

(define check-declaration-types!
  (lambda (declaration-var declarations)
    (let ([type (cadr (declaration-exp->var-type (car declarations)))])
      (for-each
        (lambda (declaration)
          (let ([declaration-type (cadr (declaration-exp->var-type declaration))])
            (check-if-types-match!
              declaration-var
              type
              declaration-type)))
        declarations))))

(define check-if-types-match!
  (lambda (declaration-var ty1 ty2)
    (if (equal? ty1 ty2)
      (void)
      (eopl:error
        'syntax-error
        (string-append
          "Types don't match in "
          (symbol->string declaration-var)
          " declaration: "
          (format "~s != ~s\n" (type-to-external-form ty2) (type-to-external-form ty1)))))))
