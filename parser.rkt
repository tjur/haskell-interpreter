#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/cfg-parser
         syntax/readerr)

(require "datatypes.rkt")

(provide scan&parse)


(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:big-letter (:/ #\A #\Z)]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [lex:comment (:: (:* lex:whitespace) "--" (:* (:~ #\newline)) #\newline)]
  [lex:identifier (:: lex:letter (:* (:or lex:letter lex:digit "_" "-")))])

(define-tokens non-terminals (
                              <integer>
                              <boolean>
                              <undefined>
                              <identifier>
                              <big-letter-name>
                              <arith-sym>
                              PLUSPLUS COLON SEMICOLON
                              OPENB CLOSEB OPENSB CLOSESB COMMA
                              IF THEN ELSE
                              LAMBDA ARROW
                              LET IN EQUALS
                              HEAD TAIL
                              DATA BAR
                              EOF))
  
(define lex
  (lexer
   [lex:whitespace (lex input-port)] ;; skip whitespace
   [lex:comment (lex input-port)] ;; skip comment 
   ["++" (token-PLUSPLUS '++)]
   ["(" (token-OPENB 'openb)]
   [")" (token-CLOSEB 'closeb)]
   ["[" (token-OPENSB 'opensb)]
   ["]" (token-CLOSESB 'closesb)]
   ["," (token-COMMA 'comma)]
   [":" (token-COLON ':)]
   [";" (token-SEMICOLON 'semicolon)]
   ["=" (token-EQUALS '=)]
   ["\\" (token-LAMBDA '\\)]
   ["->" (token-ARROW '->)]
   ["|" (token-BAR 'BAR)]
   ["True" (token-<boolean> #t)]
   ["False" (token-<boolean> #f)]
   ["undefined" (token-<undefined> 'undefined)]
   ["if" (token-IF 'if)]
   ["then" (token-THEN 'then)]
   ["else" (token-ELSE 'else)]
   ["let" (token-LET 'let)]
   ["in" (token-IN 'in)]
   ["head" (token-HEAD 'head)]
   ["tail" (token-TAIL 'tail)]
   ["data" (token-DATA 'data)]
   [(:or "+" "-" "/" "*") (token-<arith-sym> (string->symbol lexeme))]
   [(:: (:? #\-) (:+ lex:digit)) (token-<integer> (string->number lexeme))] ;; integer regexp
   [(:: lex:big-letter (:* (:or lex:letter lex:digit "_" "-"))) (token-<big-letter-name> (string->symbol lexeme))]
   [lex:identifier (token-<identifier> (string->symbol lexeme))] ;; identifier regexp
   [(eof) (token-EOF 'eof)]))
  
(define parse
  (cfg-parser
   (tokens non-terminals)
   (start <program>)
   (end EOF)
   (error (lambda (a b stx) 
            (error 'parse "failed at ~s" stx)))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Grammar ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (grammar
    [<program> [(<global-expression> <global-expressions>) (a-program (cons $1 $2))]]

    [<identifiers>  [() '()]
                    [(<identifier> <identifiers>) (cons $1 $2)]]

    [<global-expression>  [(<expression>) $1]
                          [(<declaration-exp>) $1]]

    [<global-expressions> [() '()]
                          [(SEMICOLON <global-expression> <global-expressions>) (cons $2 $3)]]

    ;; expression
    [<expression> [(<identifier>) (var-exp $1)]
                  [(HEAD <expression>) (head-exp $2)]
                  [(TAIL <expression>) (tail-exp $2)]
                  [(OPENB <expression> CLOSEB) $2]
                  [(<value-exp>) $1]
                  [(<if-exp>) $1]
                  [(<lambda-exp>) $1]
                  [(<call-exp>) $1]
                  [(<let-exp>) $1]
                  [(<infix-operator>) $1]
                  [(<data-exp>) $1]]

    ;; value
    [<value-exp>  [(<integer>) (const-exp $1)]
                  [(<boolean>) (bool-exp $1)]
                  [(<undefined>) (undefined-exp)]
                  [(OPENSB <list-exp> CLOSESB) (list-exp $2)]]

    ;; if
    [<if-exp> [(IF <expression> THEN <expression> ELSE <expression>) (if-exp $2 $4 $6)]]

    ;; lambda
    [<lambda-exp> [(LAMBDA <identifier> <identifiers> ARROW <expression>) (lambda-exp (cons $2 $3) $5)]]

    ;; let
    [<let-exp> [(LET <identifier> <identifiers> EQUALS <expression> IN <expression>) (let-exp $2 $3 $5 $7)]]

    ;; application
    [<call-exp> [(<expression> <one-or-more-expressions>) (call-exp $1 $2)]]

    [<one-or-more-expressions> [(<expression>) (list $1)]
                               [(<expressions>) $1]]

    [<expressions> [() '()]
                   [(<expression> <expressions>) (cons $1 $2)]]
            
    ;; lists
    [<list-exp> [() '()]
                [(<expression>) (list $1)]
                [(<expression> COMMA <list-exp>) (cons $1 $3)]]

    ;; infix operators
    [<infix-operator> [(<expression> <arith-sym> <expression>) (arith-exp $2 $1 $3)]
                      [(<expression> COLON <expression>) (cons-exp $1 $3)]
                      [(<expression> PLUSPLUS <expression>) (append-exp $1 $3)]]

    ;; algebraic data types (without polymorphism)
    [<data-exp> [(DATA <big-letter-name> EQUALS <val-constructor> <val-constructors>) (data-exp $2 (cons $4 $5))]]

    [<val-constructor> [(<big-letter-name> <types>) (val-constr $1 $2)]]

    [<val-constructors> [() '()]
                        [(BAR <val-constructor> <val-constructors>) (cons $2 $3)]]

    [<types> [() '()]
             [(<big-letter-name> <types>) (cons $1 $2)]]

    ;; global delarations
    [<declaration-exp> [(<identifier> <arguments> EQUALS <expression>) (declaration-exp $1 $2 $4)]]

    [<argument> [(<value-exp>) $1]
                [(<big-letter-name>) (unpack-exp $1 '())]
                [(OPENB <big-letter-name> <arguments> CLOSEB) (unpack-exp $2 $3)]
                [(<identifier>) (var-exp $1)]]

    [<arguments>  [() '()]
                  [(<argument> <arguments>) (cons $1 $2)]]

    )))


(define scan&parse
  (lambda (str)
    (let ([p (open-input-string str)])
      (parse (lambda () (lex p))))))

(scan&parse "let f x = x in (f 5)")

(scan&parse "\\x y -> (x + y)")

(scan&parse "data Tree = Empty | Leaf Int | Node Tree Tree")

(scan&parse "[] ++ [x, 5, 10]")

(scan&parse "f 0 = 1; f n = n * (factorial (n - 1))")

(scan&parse "rev acc [] = acc; rev acc xs = rev ((head xs):acc) xs")

(scan&parse "f Leaf = 0; f (Node l x r) = (f l) + x + (f r)")
