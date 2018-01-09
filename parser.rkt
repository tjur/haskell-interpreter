#lang racket

(require parser-tools/cfg-parser)
(provide (all-from-out parser-tools/cfg-parser))

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         syntax/readerr)

(require "datatype.rkt")

(define-lex-abbrevs [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [lex:comment (:: (:* lex:whitespace) "--" (:* (:~ #\newline)) #\newline)]
  [lex:identifier (:: lex:letter (:* (:or lex:letter lex:digit "_" "-")))])

(define-tokens non-terminals (
                              <integer>
                              <boolean>
                              <undefined>
                              <identifier>
                              <arith-sym>
                              PLUSPLUS COLON
                              OPENSB CLOSESB COMMA
                              EOF
                              IF THEN ELSE
                              LAMBDA ARROW
                              LET IN EQUALS
                              HEAD TAIL))
  
(define lex
  (lexer
   ["++" (token-PLUSPLUS '++)]
   ["[" (token-OPENSB 'opensb)]
   ["]" (token-CLOSESB 'closesb)]
   ["," (token-COMMA 'comma)]
   [":" (token-COLON ':)]
   ["=" (token-EQUALS '=)]
   ["\\" (token-LAMBDA '\\)]
   ["->" (token-ARROW '->)]
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
   [(:or "+" "-" "/" "*") (token-<arith-sym> (string->symbol lexeme))]
   [(:: (:? #\-) (:+ lex:digit)) (token-<integer> (string->number lexeme))] ;; integer regexp
   [whitespace (lex input-port)] ;; skip whitespace
   [lex:comment (lex input-port)] ;; skip comment
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
   (grammar [<program> [(<expression>) (a-program $1)]]
            [<expression> [(<integer>) (const-exp $1)]
                          [(<boolean>) (bool-exp $1)]
                          [(<undefined>) (undefined-exp)]
                          [(<identifier>) (var-exp $1)]
                          [(OPENSB <list-exp> CLOSESB) (list-exp $2)]
                          [(HEAD <expression>) (head-exp $2)]
                          [(TAIL <expression>) (tail-exp $2)]
                          [(<if-exp>) $1]
                          [(<lambda-exp>) $1]
                          [(<call-exp>) $1]
                          [(<let-exp>) $1]
                          [(<infix-operator>) $1]]

            ;; if
            [<if-exp> [(IF <expression> THEN <expression> ELSE <expression>) (if-exp $2 $4 $6)]]

            ;; lambda
            [<lambda-exp> [(LAMBDA <identifier> ARROW <expression>) (lambda-exp $2 $4)]]
            [<let-exp> [(LET <identifier> <identifiers> EQUALS <expression> IN <expression>) (let-exp $2 $3 $5 $7)]]

            [<identifiers>  [() '()]
                            [(<identifier> <identifiers>) (cons $1 $2)]]

            ;; application
            [<call-exp> [(<expression> <one-or-more-expressions>) (call-exp $1 $2)]]

            [<one-or-more-expressions> [(<expression>) (list $1)]
                                       [(<expressions>) $1]]

            [<expressions> [() '()]
                           [(<expression> <expressions>) (cons $1 $2)]]
            
            ;; lists
            [<list-exp> [() (empty-list-exp)]
                        [(<expression>) (cons-list-exp $1 (empty-list-exp))]
                        [(<expression> COMMA <list-exp>) (cons-list-exp $1 $3)]]

            [<infix-operator> [(<expression> <arith-sym> <expression>) (arith-exp $2 $1 $3)]
                              [(<expression> COLON <expression>) (cons-exp $1 $3)]
                              [(<expression> PLUSPLUS <expression>) (append-exp $1 $3)]]


            )))

(let ([p (open-input-string "let f x = x in f 5")])
  (parse (lambda () (lex p))))
