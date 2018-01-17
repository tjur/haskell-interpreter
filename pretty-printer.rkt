#lang eopl

(require (only-in racket/string
                  string-join))

(require "datatypes.rkt")
(require "store.rkt") ;; wyświetlanie elementów listy

(provide pretty-print-program pretty-print-exp pretty-print-expval)

(define indent
  (lambda (n)
    (if (eq? n 0)
      ""
      (string-append "    " (indent (- n 1))))))

(define pretty-print-program
  (lambda (prog)
    (cases program prog
      (a-program (exps)
        (string-join
          (map pretty-print-exp exps)
          "\n"
          #:after-last "\n")))))

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

          (if-exp (exp1 exp2 exp3)
            (string-append
              "if " ((pretty-exp indents_1) exp1) "\n"
              (indent indents) "then " ((pretty-exp indents_1) exp2) "\n"
              (indent indents) "else " ((pretty-exp indents_1) exp3)))
          
          (lambda-exp (vars body)
            (string-append
              "\\" (string-join (map symbol->string vars) " ") " -> " ((pretty-exp indents) body)))

          (call-exp (rator rand)
            (string-append
              "(" ((pretty-exp indents) rator) ((pretty-exp indents) rand) ")"))

          (let-exp (vars exps body)
            (let ((one-let (lambda (var args exp)
                            (string-append
                              (string-join (map symbol->string (cons var args)) " ") " = " ((pretty-exp indents_2) exp)))))
              (string-append
                "let " (string-join (map one-let vars exps) (string-append "\n" (indent indents_1))) "\n"
                (indent indents) "in " ((pretty-exp indents_1) body))))

          (cons-exp (head tail)
            (string-append
              "(" ((pretty-exp indents) head) ":" ((pretty-exp indents) tail) ")"))

          (data-exp (type vals)
            (string-append
              "data " (symbol->string type) " = " (string-join (map pretty-print-val-constr vals) " | ")))

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
              (call-exp (var-exp proc) (list exp1)))))))))


(define pretty-print-val-constr
  (lambda (val)
    (cases val-constr-exp val
      (val-constr (name types)
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

      ;;; (list-val (items)
      ;;;   (string-join
      ;;;     (map pretty-print-expval items)
      ;;;     ", "
      ;;;     #:before-first "["
      ;;;     #:after-last "]"))

      (list-val (_) "[...]")

      ;; TODO: wypisywanie enva!
      (proc-val (p)
        (cases proc p
          (procedure (bvar body env)
            (string-append
              "\\" (symbol->string bvar) " -> " ((pretty-exp 1) body))))))))
