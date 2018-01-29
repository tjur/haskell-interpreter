#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/cfg-parser)

(require "datatypes.rkt")
(require "pretty-printer.rkt")
(require "data-expression.rkt")

(provide scan&parse cons3 create-let-exp)


(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:big-letter (:/ #\A #\Z)]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [lex:comment (:: (:* lex:whitespace) "--" (:* (:~ #\newline)) #\newline)]
  [lex:identifier (:: (:or lex:letter "_") (:* (:or lex:letter lex:digit "_" "-")))]
  [lex:symbol (:or "!" "#" "$" "%" "&" "*" "+" "." "/" "<" "=" ">" "?" "@" "\\" "^" "|" "-" "~" ":")])

(define-empty-tokens empty-tokens (
                                   <unit>
                                   <int-type> <bool-type> <unit-type> <int-list-type>
                                   COLON DOUBLECOLON SEMICOLON COMMA GRAVE
                                   OPENB CLOSEB OPENSB CLOSESB
                                   IF THEN ELSE
                                   LAMBDA ARROW
                                   LET IN EQUALS
                                   DATA BAR
                                   EOF))

(define-tokens non-empty-tokens (
                                 <integer>
                                 <boolean>
                                 <identifier>
                                 <big-letter-name>
                                 <operator>))
  
(define lex
  (lexer
   [lex:whitespace (lex input-port)] ;; skip whitespace
   [lex:comment (lex input-port)] ;; skip comment
   ["()" (token-<unit>)]
   ["(" (token-OPENB)]
   [")" (token-CLOSEB)]
   ["[" (token-OPENSB)]
   ["]" (token-CLOSESB)]
   ["," (token-COMMA)]
   ["`" (token-GRAVE)]
   ["::" (token-DOUBLECOLON)]
   [":" (token-COLON)]
   [";" (token-SEMICOLON)]
   ["=" (token-EQUALS)]
   ["\\" (token-LAMBDA)]
   ["->" (token-ARROW)]
   ["|" (token-BAR)]
   ["int" (token-<int-type>)]
   ["bool" (token-<bool-type>)]
   ["unit" (token-<unit-type>)]
   ["int-list" (token-<int-list-type>)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["let" (token-LET)]
   ["in" (token-IN)]
   ["data" (token-DATA)]
   ["True" (token-<boolean> #t)]
   ["False" (token-<boolean> #f)]
   [(:: lex:symbol (:* lex:symbol)) (token-<operator> (string->symbol lexeme))] 
   [(:: (:? #\-) (:+ lex:digit)) (token-<integer> (string->number lexeme))] ;; integer regexp
   [(:: lex:big-letter (:* (:or lex:letter lex:digit "_" "-"))) (token-<big-letter-name> (string->symbol lexeme))]
   [lex:identifier (token-<identifier> (string->symbol lexeme))] ;; identifier regexp
   [(eof) (token-EOF)]))
  
(define parse
  (cfg-parser
   (tokens empty-tokens non-empty-tokens)
   (start <program>)
   (end EOF)
   (error (lambda (a b stx) 
            (error 'parse "failed at ~s" stx)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grammar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (grammar
    [<program> [(<global-expression> <global-expressions>) (a-program (cons $1 $2))]]

    [<identifiers-with-types>  [() '()]
                    [(<identifier-with-type> <identifiers-with-types>) (cons $1 $2)]]

    [<identifier-with-type> [(OPENB <big-letter-name> DOUBLECOLON <type> CLOSEB) (list $2 $4)]
                            [(OPENB <identifier> DOUBLECOLON <type> CLOSEB) (list $2 $4)]]

    [<operator-with-type> [(OPENB OPENB <operator> CLOSEB DOUBLECOLON <type> CLOSEB) (list $3 $6)]]

    ;; types
    [<type> [(OPENB <type> CLOSEB) $2]
            [(<int-type>) (int-type)]
            [(<bool-type>) (bool-type)]
            [(<unit-type>) (unit-type)]
            [(<int-list-type>) (int-list-type)]
            [(<type> ARROW <type>) (proc-type $1 $3)]
            [(<big-letter-name>) (create-data-exp-type $1)]
            [(<identifier>) (create-data-exp-type $1)]]

    [<types> [() '()]
             [(<type> <types>) (cons $1 $2)]]

    [<global-expression>  [(<expression>) $1]
                          [(<declaration-exp>) $1]
                          [(<data-exp>) $1]]

    [<global-expressions> [() '()]
                          [(SEMICOLON <global-expression> <global-expressions>) (cons $2 $3)]]

    ;; expression
    [<expression> [(OPENB <expression> CLOSEB) $2]
                  [(<value-exp>) $1]
                  [(<var-exp>) $1]
                  [(<if-exp>) $1]
                  [(<lambda-exp>) $1]
                  [(<call-exp>) $1]
                  [(<let-exp>) $1]
                  [(<infix-op-exp>) $1]]

    ;; simple values
    [<value-exp>  [(<integer>) (const-num-exp $1)]
                  [(<boolean>) (const-bool-exp $1)]
                  [(<unit>) (unit-exp)]
                  [(OPENSB <list-exp> CLOSESB) (list-exp $2)]]

    ;; single variable
    [<var-exp> [(<identifier>) (var-exp $1)]
               [(<big-letter-name>) (var-exp $1)]
               [(OPENB <operator> CLOSEB) (var-exp $2)]]

    [<var-exp-without-big-letter-name> [(<identifier>) (var-exp $1)]
                                       [(OPENB <operator> CLOSEB) (var-exp $2)]]

    ;; if
    [<if-exp> [(IF <expression> THEN <expression> ELSE <expression>) (if-exp $2 $4 $6)]]

    ;; lambda
    [<lambda-exp> [(LAMBDA <identifier-with-type> <identifiers-with-types> ARROW <expression>) (to-one-arg-proc (cons $2 $3) $5)]]

    ;; let
    [<let-exp> [(LET <let-def> <let-defs> IN <expression>) (create-let-exp (cons3 $2 $3) $5)]]

    [<let-def> [(<identifier-with-type> <identifiers-with-types> EQUALS <expression>) (list $1 $2 $4)]]

    [<let-defs> [() (list '() '() '())]
                [(<let-def> <let-defs>) (cons3 $1 $2)]]

    ;; application
    [<call-exp> [(<expression> <expression>) (call-exp $1 $2)]
                [(<expression> GRAVE <var-exp> GRAVE <expression>) (call-exp (call-exp $3 $1) $5)]]

    [<one-or-more-expressions> [(<expression>) (list $1)]
                               [(<expressions>) $1]]

    [<expressions> [() '()]
                   [(<expression> <expressions>) (cons $1 $2)]]
            
    ;; lists
    [<list-exp> [() '()]
                [(<expression>) (list $1)]
                [(<expression> COMMA <list-exp>) (cons $1 $3)]]

    ;; infix operators
    [<infix-op-exp> [(<expression> COLON <expression>) (cons-exp $1 $3)]
                    [(<expression> <operator> <expression>) (call-exp (call-exp (var-exp $2) $1) $3)]]

    ;; data expression - algebraic data types (without polymorphism)
    [<data-exp> [(DATA <big-letter-name> EQUALS <val-constructor> <val-constructors>) (a-data-exp (data-exp-type $2) (cons $4 $5))]]

    [<val-constructor> [(<big-letter-name> <types-or-identifiers>) (a-val-constr $1 $2)]]

    [<val-constructors> [() '()]
                        [(BAR <val-constructor> <val-constructors>) (cons $2 $3)]]

    [<types-or-identifiers> [() '()]
                            [(<type-or-identifier> <types-or-identifiers>) (cons $1 $2)]]

    [<type-or-identifier> [(<type>) $1]
                          [(<big-letter-name>) (data-exp-type $1)]
                          [(<identifier>) (data-exp-type $1)]]
    

    ;; global declarations
    [<declaration-exp> [(<identifier-with-type> <arguments-with-type> EQUALS <expression>) (declaration-exp $1 $2 $4)]
                       [(<operator-with-type>   <arguments-with-type> EQUALS <expression>) (declaration-exp $1 $2 $4)]]

    [<argument> [(<value-exp>) $1]
                [(<argument> COLON <argument>) (unpack-exp ': (list $1 $3))]
                [(<big-letter-name> <arguments>) (unpack-exp $1 $2)]
                [(<var-exp-without-big-letter-name>) $1]
                [(OPENB <argument> CLOSEB) $2]]

    [<arguments> [() '()]
                 [(<argument> <arguments>) (cons $1 $2)]]

    [<arguments-with-type>  [() '()]
                [(<argument-with-type> <arguments-with-type>) (cons $1 $2)]]

    [<argument-with-type> [(OPENB <argument> DOUBLECOLON <type> CLOSEB) (list $2 $4)]]

    )))


;; auxiliary procedures
(define cons3
  (lambda (xs xss)
    (list
     (cons (list-ref xs 0) (list-ref xss 0))
     (cons (list-ref xs 1) (list-ref xss 1))
     (cons (list-ref xs 2) (list-ref xss 2)))))

(define (to-one-arg-proc b-vars-with-types p-body)
  (define to-one-arg-proc-aux
         (lambda (rev-b-vars-with-types curr-p-body)
           (if (null? rev-b-vars-with-types)
               curr-p-body
               (to-one-arg-proc-aux
                (cdr rev-b-vars-with-types)
                (let* ([res (car rev-b-vars-with-types)]
                       [b-var (list-ref res 0)]
                       [b-var-type (list-ref res 1)])
                  (lambda-exp b-var b-var-type curr-p-body))))))
  
  (to-one-arg-proc-aux (reverse b-vars-with-types) p-body))

(define create-let-exp
  (lambda (let-defs let-body)
    (let* [(p-names-with-result-types (list-ref let-defs 0))
           (p-names (map (lambda (x) (list-ref x 0)) p-names-with-result-types))
           (p-result-types (map (lambda (x) (list-ref x 1)) p-names-with-result-types))
           (ps-vars-with-types (list-ref let-defs 1))
           (ps-vars (map (lambda (x)
                           (map (lambda (b-var-with-type)(list-ref b-var-with-type 0)) x)) ps-vars-with-types))
           (ps-vars-types (map (lambda (x)
                                 (map (lambda (b-var-with-type)(list-ref b-var-with-type 1)) x)) ps-vars-with-types))
           (p-bodies (list-ref let-defs 2))
           (new-p-bodies (map to-one-arg-proc ps-vars-with-types p-bodies))]
    (let-exp
     p-names
     p-result-types
     ps-vars
     ps-vars-types
     new-p-bodies
     let-body))))

(define create-data-exp-type
  (lambda (type-name)
    (let ([new-type (data-exp-type type-name)])
    (begin
      (add-type-to-check-list! new-type) ;; we will check if the type is real type later
      new-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scan&parse
  (lambda (str)
    (let ([p (open-input-string str)])
      (parse (lambda () (lex p))))))

(define pretty-print-programs
  (lambda (progs)
    (for-each
      (lambda (prog)
        (display
          (pretty-print-program
            (scan&parse prog)))
        (newline))
      progs)))
