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
  [lex:comment (:: (:* lex:whitespace) "--" (:* (:~ #\newline)) #\newline)])

(define-tokens non-terminals (
                              <arith-sym>
                              PLUSPLUS COLON
                              EOF
                              IF THEN ELSE
                              OPENSB CLOSESB COMMA
                              HEAD TAIL))

  
(define lex
  (lexer
   ["++" (token-PLUSPLUS '++)]
   [(:or "+" "-" "/" "*") (token-<arith-sym> (string->symbol lexeme))]
   ["[" (token-OPENSB 'opensb)]
   ["]" (token-CLOSESB 'closesb)]
   ["," (token-COMMA 'comma)]
   [":" (token-COLON ':)]
   ["if" (token-IF 'if)]
   ["then" (token-THEN 'then)]
   ["else" (token-ELSE 'else)]
   ["head" (token-HEAD 'head)]
   ["tail" (token-TAIL 'tail)]
   [whitespace (lex input-port)] ;; skip whitespace
   [lex:comment (lex input-port)] ;; skip comment
   [(eof) (token-EOF 'eof)]))
  
(define parse
  (cfg-parser
   (tokens non-terminals)
   (start <program>)
   (end EOF)
   (error (lambda (a b stx) 
            (error 'parse "failed at ~s" stx)))
   (grammar [<program> [(<expression>) (a-program $1)]]
            [<expression> [(OPENSB <list-exp> CLOSESB) (list-exp $2)]
                          [(HEAD <expression>) (head-exp $2)]
                          [(TAIL <expression>) (tail-exp $2)]
                          [(<if-exp>) $1]
                          [(<infix-operator>) $1]]
            [<if-exp> [(IF <expression> THEN <expression> ELSE <expression>) (if-exp $2 $4 $6)]]
            [<list-exp> [() (empty-list-exp)]
                        [(<expression>) (cons-list-exp $1 (empty-list-exp))]
                        [(<expression> COMMA <list-exp>) (cons-list-exp $1 $3)]]
            [<infix-operator> [(<expression> <arith-sym> <expression>) (arith-exp $2 $1 $3)]
                              [(<expression> COLON <expression>) (cons-exp $1 $3)]
                              [(<expression> PLUSPLUS <expression>) (append-exp $1 $3)]]

            )))

(let ([p (open-input-string "[] + []")])
  (parse (lambda () (lex p))))
