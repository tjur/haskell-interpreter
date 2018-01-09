
#lang eopl

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
  
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    
    (comment ("--" (arbno (not #\newline))) skip)
    
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
    
    ))


(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)

    (expression (identifier) var-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("let"
      identifier (arbno identifier) "=" expression
       (arbno "\n" identifier (arbno identifier) "=" expression) "in" expression)
     let-exp)

    (expression
     ("\\" identifier "->" expression)
     lambda-exp)

    ;; Lists
    
    (expression
     ("[" (separated-list expression ",") "]")
     list-exp)

    (expression ("head" expression) head-exp)

    (expression ("tail" expression) tail-exp)

    ;; Infix operators
    (expression
     ("(" expression infix-op-expression ")")
     infix-op-exp)

    (infix-op-expression () empty-exp)
    (infix-op-expression (":" expression) cons-exp)
    (infix-op-expression ("++" expression) append-op)
    (infix-op-expression ("+" expression) add-op)
    (infix-op-expression ("-" expression) subt-op)
    (infix-op-expression ("*" expression) mul-op)
    (infix-op-expression ("/" expression) div-op)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
  
(sllgen:make-define-datatypes the-lexical-spec the-grammar)
  
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
  
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))
  
(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
