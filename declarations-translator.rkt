#lang eopl

(require (only-in racket/base foldr))
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




(define same-arguments?
  (lambda (arg1 arg2)
    (cases expression arg1
      (var-exp (var1)
        (cases expression arg2
          (var-exp (var2)
            #t)
          (else #f)))
      
      (else (equal? arg1 arg2)))))  ;; TODO: lists? & Datatypes

(define-datatype argument argument?
  (an-argument
    (exp expression?)
    (arguments (list-of argument?)))
  (ending-argument
    (exp expression?)
    (body expression?)))

(define argument->exp
  (lambda (arg)
    (cases argument arg
      (an-argument (exp args) exp)
      (ending-argument (exp body) exp))))

;; get-same-and-rest : List(Exp) x List(List(Exp)) x List(Exp) -> (List(List(Exp)) x List(Exp)) x (List(List(Exp)) x List(Exp))
(define get-same-and-rest
  (lambda (args arguments-list bodies)
    (if (null? arguments-list) (cons (cons '() '()) (cons '() '()))
      (let* ([result (get-same-and-rest args (cdr arguments-list) (cdr bodies))]
             [same (car result)]
             [same-arguments (car same)]
             [same-bodies (cdr same)]
             [rest (cdr result)]
             [rest-arguments (car rest)]
             [rest-bodies (cdr rest)])
        (if (same-arguments? (car args) (caar arguments-list))
          (cons
            (cons
              (cons (car arguments-list) same-arguments)
              (cons (car bodies) same-bodies))
            rest)

          (cons
            same
            (cons
              (cons (car arguments-list) rest-arguments)
              (cons (car bodies) rest-bodies))))))))


(define group-it
  (lambda (arguments-list bodies)
    (if (null? arguments-list) '()
      (let* ([arguments (car arguments-list)]
             [result (get-same-and-rest arguments arguments-list bodies)]
             [same (car result)]
             [same-arguments (car same)]
             [same-bodies (cdr same)]
             [rest (cdr result)]
             [rest-arguments (car rest)]
             [rest-bodies (cdr rest)])
        (cons
          (if (null? (cdr arguments))
            (ending-argument
              (car arguments)
              (car same-bodies))
            (an-argument
              (car arguments)
              (group-it
                (map cdr same-arguments)
                same-bodies)))
          
          (group-it
            rest-arguments
            rest-bodies))))))

(define get-pattern-var
  (lambda (pattern-var-n)
    (string->symbol (string-append "_$" (number->string pattern-var-n) "_"))))

(define translate-some
  (lambda (pattern-var pattern-var-n fkin-grouped)
    (if (null? fkin-grouped) (missing-case-exp)
      (let* ([arg (car fkin-grouped)]
            [next-pattern-var (get-pattern-var (+ pattern-var-n 1))])
        (cases argument arg
          (an-argument (pattern-exp grouped-args)
            (let* ([next-body (lambda-exp
                                next-pattern-var
                                (any-type)
                                (translate-some
                                  next-pattern-var
                                  (+ pattern-var-n 1)
                                  grouped-args))]
                  [simple-match (lambda ()
                                  (if-exp
                                    (call-exp
                                      (call-exp (var-exp '==) (var-exp pattern-var))
                                      pattern-exp)
                                    next-body
                                    (translate-some
                                      pattern-var
                                      pattern-var-n
                                      (cdr fkin-grouped))))])

              (cases expression pattern-exp
                (const-num-exp (n)
                  (simple-match))

                (const-bool-exp (b)
                  (simple-match))

                (unit-exp ()
                  (simple-match))

                ;; TODO: lists & Datatypes

                (var-exp (var)
                  (let-exp (list var) (list (any-type)) (list (var-exp pattern-var))
                    next-body))
                
                (else (eopl:error "SOME ERRA")))))

          (ending-argument (pattern-exp body)
            (let ([simple-match (lambda ()
                                  (if-exp
                                    (call-exp
                                      (call-exp (var-exp '==) (var-exp pattern-var))
                                      pattern-exp)
                                    body
                                    (translate-some
                                      pattern-var
                                      pattern-var-n
                                      (cdr fkin-grouped))))])
              (cases expression pattern-exp
                (const-num-exp (n)
                  (simple-match))

                (const-bool-exp (b)
                  (simple-match))

                (unit-exp ()
                  (simple-match))

                ;; TODO: lists & Datatypes

                (var-exp (var)
                  (let-exp (list var) (list (any-type)) (list (var-exp pattern-var))
                    body))
                
                (else (eopl:error "SOME ERRA"))))))))))

(define get-same-and-rest-declarations
  (lambda (var declarations)
    (if (null? declarations) (cons '() '())
      (let* ([declaration (car declarations)]
             [result (get-same-and-rest-declarations var (cdr declarations))]
             [same (car result)]
             [rest (cdr result)])
        (cases expression declaration
          (declaration-exp (var1 arguments body)
            (if (eq? var var1)
              (cons
                (cons declaration same)
                rest)
              
              (cons
                same
                (cons declaration rest))))
          (else (eopl:error "THATS IMPOSSIBLE!")))))))

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

(define split-declarations-to-args-bodies
  (lambda (declarations)
    (if (null? declarations) (cons '() '())
      (let ([declaration (car declarations)])
        (cases expression declaration
          (declaration-exp (var arguments body)
            (let* ([result (split-declarations-to-args-bodies (cdr declarations))]
                   [args (car result)]
                   [bodies (cdr result)])
              (cons
                (cons arguments args)
                (cons body bodies))))
          (else (eopl:error "THATS IMPOSSIBLE!")))))))

(define translate-to-let-without-body
  (lambda (declarations)
    (let* ([declarations-grouped (group-declarations-by-var declarations)]
           [groups (map (lambda (group)
                          (let ([var (car group)]
                                [declarations (cdr group)]
                                [args-bodies (split-declarations-to-args-bodies declarations)])
                            (list
                              var
                              (any-type)
                              (translate-some2 (car args-bodies) (cdr args-bodies)))))
                        declarations-grouped)]
      ;;; groups)))
           [joined (join3 groups)])
      (no-body-let
        (list-ref joined 0)
        (list-ref joined 1)
        (list-ref joined 2)))))

(define translate-some2
  (lambda (arguments-list bodies)
    (if (null? (car arguments-list))  ;; pusta lista argumentów
      (car bodies)
      (lambda-exp
        (get-pattern-var 0)
        (any-type)
        (translate-some
          (get-pattern-var 0)
          0
          (group-it arguments-list bodies))))))

(define join3
  (lambda (lst)
    (if (null? lst) (list '() '() '())
      (let* ([result (join3 (cdr lst))]
             [hd (car lst)])
        (list
          (cons (list-ref hd 0) (list-ref result 0))
          (cons (list-ref hd 1) (list-ref result 1))
          (cons (list-ref hd 2) (list-ref result 2)))))))
