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
                              <integer>
                              <boolean>
                              <identifier>
                              PLUS MINUS TIMES
                              IF THEN ELSE
                              LAMBDA ARROW
                              LET IN EQUALS
                              EOF))

  
(define lex
  (lexer
   ["True" (token-<boolean> #t)]
   ["False" (token-<boolean> #f)]
   [(:: (:? #\-) (:+ lex:digit)) (token-<integer> (string->number lexeme))] ;; integer regexp
   ["if" (token-IF 'if)]
   ["then" (token-THEN 'then)]
   ["else" (token-ELSE 'else)]
   ["\\" (token-LAMBDA '\\)]
   ["->" (token-ARROW '->)]
   ["let" (token-LET 'let)]
   ["in" (token-IN 'in)]
   ["=" (token-EQUALS '=)]
   [whitespace (lex input-port)] ;; skip whitespace
   [lex:comment (lex input-port)] ;; skip comment
   [(:: lex:letter (:* (:or lex:letter lex:digit))) (token-<identifier> (string->symbol lexeme))] ;; identifier regexp
   [(eof) (token-EOF 'eof)]))
  
(define parse
  (cfg-parser
   (tokens non-terminals)
   (start <program>)
   (end EOF)
   (error (lambda (a b stx) 
            (error 'parse "failed at ~s" stx)))
   (grammar [<program> [(<expression>) (a-program $1)]]
            
            [<expression> [(<integer>) (const-exp $1)]
                          [(<boolean>) (bool-exp $1)]
                          [(<identifier>) (var-exp $1)]
                          [(<if-exp>) $1]
                          [(<lambda-exp>) $1]
                          [(<let-exp>) $1]]
            
            [<if-exp> [(IF <expression> THEN <expression> ELSE <expression>) (if-exp $2 $4 $6)]]

            [<lambda-exp> [(LAMBDA <identifier> ARROW <expression>) (lambda-exp $2 $4)]]

             

            )))

(let ([p (open-input-string "if True then 42 else 100")])
  (parse (lambda () (lex p))))
