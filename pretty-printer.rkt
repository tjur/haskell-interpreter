#lang eopl

(require (only-in racket/string
                  string-join))
(require (only-in racket/base
                  format))

(require "datatypes.rkt")
(require "store.rkt")


(provide pretty-print-program pretty-print-exp pretty-print-expval pretty-print-exp-result)

(define indent
  (lambda (n)
    (if (eq? n 0)
      ""
      (string-append "    " (indent (- n 1))))))

(define pretty-print-exp-result
  (lambda (val ty i)
    (string-append
      (format "Expression ~s\n" i)
      (string-append (pretty-print-expval val) (format " :: ~s\n\n" (type-to-external-form ty))))))

(define pretty-print-program
  (lambda (prog)
    (cases program prog
      (a-program (exps)
        (string-join
          (map pretty-print-exp exps)
          "\n"
          #:after-last "\n")))))

(define pretty-print-data-exp
  (lambda (exp)
    (cases data-exp exp
      (a-data-exp (type vals)
                  (string-append
                   "data " (symbol->string type) " = " (string-join (map pretty-print-val-constr vals) " | "))))))

(define pretty-print-exp
  (lambda (exp)
    ((pretty-exp 0) exp)))

(define pretty-exp
  (lambda (indents)
    (lambda (exp)
      (let ((indents_1 (+ indents 1))
            (indents_2 (+ indents 2)))
        (cases expression exp
          (const-num-exp (n)
            (number->string n))

          (const-bool-exp (b)
            (if b
              "True"
              "False"))

          (unit-exp ()
            "()")

          (var-exp (var)
            (symbol->string var))

          (list-exp (exps)
            (string-join
              (map (pretty-exp indents) exps)
              ", "
              #:before-first "["
              #:after-last "]"))

          (type-value-exp (val-constr-name b-vars b-vars-types ty)
                          (string-append
                           val-constr-name " "
                           (string-join (map symbol->string b-vars) ", " #:before-first "(" #:after-last ")")))

          (if-exp (exp1 exp2 exp3)
            (string-append
              "if " ((pretty-exp indents_1) exp1) "\n"
              (indent indents) "then " ((pretty-exp indents_1) exp2) "\n"
              (indent indents) "else " ((pretty-exp indents_1) exp3)))
          
          (lambda-exp (var type body)
            (string-append
              "\\" (symbol->string var) " -> " ((pretty-exp indents) body)))

          (call-exp (rator rand)
            (string-append
              "(" ((pretty-exp indents) rator) " " ((pretty-exp indents) rand) ")"))

          (let-exp (p-names p-result-types ps-vars ps-vars-types exps body)
            (let ((one-let (lambda (var args exp)
                            (string-append
                              (string-join (map symbol->string (cons var args)) " ") " = " ((pretty-exp indents_2) exp)))))
              (string-append
                "let " (string-join (map one-let p-names ps-vars exps) (string-append "\n" (indent indents_1))) "\n"
                (indent indents) "in " ((pretty-exp indents_1) body))))

          (cons-exp (head tail)
            (string-append
              "(" ((pretty-exp indents) head) ":" ((pretty-exp indents) tail) ")"))

          (unpack-exp (val-constr values)
            (string-append
              "(" (symbol->string val-constr) (string-join (map (pretty-exp indents) values) " " #:before-first " ") ")"))
          
          (declaration-exp (var arguments body)
            (string-append
              (symbol->string var) (string-join (map (pretty-exp indents) arguments) " " #:before-first " ") " =\n"
              (indent indents_1) ((pretty-exp indents_1) body)))

          (number-op-exp (op exp1 exp2)
            ((pretty-exp indents)
              (call-exp (var-exp op) (list exp1 exp2))))
              
          (list-proc-exp (proc exp1)
            ((pretty-exp indents)
              (call-exp (var-exp proc) (list exp1))))

          (common-op-exp (op exp1 exp2)
            ((pretty-exp indents)
              (call-exp (var-exp op) (list exp1 exp2))))

          (missing-case-exp (var) "<missing-case>")

          (check-data-exp-val-exp (exp1 val-constr-name)
                              (string-append (symbol->string val-constr-name) "? "((pretty-exp indents_1) exp1)))

          (extract-from-data-exp-val-exp (exp1 index)
                                     (string-append "(" ((pretty-exp indents_1) exp1) ")[" (number->string index) "]"))

          (empty-exp () "")

          )))))


(define pretty-print-val-constr
  (lambda (val)
    (cases val-constr-exp val
      (a-val-constr (name types)
        (string-join (map symbol->string (cons name types)) " ")))))

(define pretty-print-expval
  (lambda (val)
    (cases expval val
      (num-val (n)
        (number->string n))

      (bool-val (bool)
        (if bool
          "True"
          "False"))

      (unit-val () "()")

      (list-val (refs)
                (string-join
                 (map pretty-print-expval (flatten-list-of-refs-and-deref refs))
                 ", "
                 #:before-first "["
                 #:after-last "]"))

      (proc-val (p)
        (cases proc p
          (procedure (bvar body env)
            (string-append
              "\\" (symbol->string bvar) " -> " ((pretty-exp 1) body)))))

      (data-exp-val (val-constr-name refs type)
                    (if (null? refs)
                        (symbol->string val-constr-name)
                        (string-append
                         (symbol->string val-constr-name) " ("
                         (string-join (map (lambda (ref) (pretty-print-expval (deref ref))) refs) " ") ")")))

      )))


(define flatten-list-of-refs-and-deref
  (lambda (refs)
    (if (null? refs)
        '()
        (let ([val (deref (car refs))]
              [vals (flatten-list-of-refs-and-deref (expval->list (deref (cadr refs))))])
          (cons val vals)))))


;; type-to-external-form : Type -> List | Sym
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (unit-type () '())
      (int-list-type () 'int-list)
      (proc-type (arg-type result-type)
                 (list
                  (type-to-external-form arg-type)
                  '->
                  (type-to-external-form result-type)))
      (data-exp-type (name) name)
      
      (else (eopl:error 'type-to-external-form "Not implemented for type: ~s" ty))
      
      )))
