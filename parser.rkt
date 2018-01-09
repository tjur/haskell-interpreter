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
                              PLUS MINUS TIMES
                              EOF
                              IF THEN ELSE
                              OPENSB CLOSESB COMMA))

  
(define lex
  (lexer
   ["+" (token-PLUS '+)]
   ["-" (token-MINUS '-)]
   ["*" (token-TIMES '*)]
   ["if" (token-IF 'if)]
   ["then" (token-THEN 'then)]
   ["else" (token-ELSE 'else)]
   ["[" (token-OPENSB 'opensb)]
   ["]" (token-CLOSESB 'closesb)]
   ["," (token-COMMA 'comma)]
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
            [<expression> [(PLUS) (var-exp '+)]
                          [(MINUS) (var-exp '-)]
                          [(TIMES) (var-exp '*)]
                          [(<if-exp>) $1]
                          [(OPENSB <list-exp> CLOSESB) (list-exp $2)]]
            [<if-exp> [(IF <expression> THEN <expression> ELSE <expression>) (if-exp $2 $4 $6)]]
            [<list-exp> [() (empty-list-exp)]
                        [(<expression>) (cons-list-exp $1 (empty-list-exp))]
                        [(<expression> COMMA <list-exp>) (cons-list-exp $1 $3)]]

            )))

(let ([p (open-input-string "[+,    -, +, if + then - else *]")])
  (parse (lambda () (lex p))))
