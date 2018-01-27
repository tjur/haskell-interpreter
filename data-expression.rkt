#lang eopl

(require "datatypes.rkt")
(require "environments.rkt")
(require "store.rkt")
(require "type-checker.rkt")
(require (only-in racket/base
                  foldl reverse remove*))

(provide process-data-exps add-type-to-check-list! initialize-types-to-check-list!)


(define type-value-id 'uninitialized)

(define initialize-type-value-id!
  (lambda ()
    (set! type-value-id 0)))

(define types-to-check-if-exist 'uninitialized)

(define initialize-types-to-check-list!
  (lambda ()
    (set! types-to-check-if-exist '())))

(define add-type-to-check-list!
  (lambda (ty)
    (set! types-to-check-if-exist (cons ty types-to-check-if-exist))))

;; remove from list type that match type od given data-exp (we know that type exists)
(define check-if-types-in-list-exist
  (lambda (types)
    (let ([new-list (remove* types types-to-check-if-exist)])
      (if (null? new-list)
          42 ;; Ok - every type exist
          (if (null? (cdr new-list))
              (eopl:error 'type-error "No such type: ~s" (car new-list))
              (eopl:error 'type-error "No such types: ~s" new-list))))))
  

;; returns new env with value constructors as a procedures
(define process-data-exps
  (lambda (data-exps env)
    (begin
      (initialize-type-value-id!)
      (check-if-types-in-list-exist
       (foldl
        (lambda (d-exp types) (cases data-exp d-exp (a-data-exp (ty _) (cons ty types)))) '() data-exps))
      (foldl
       (lambda (d-exp env) (create-val-contructor-procs d-exp env))
       env
       data-exps))))


(define create-val-contructor-procs
  (lambda (d-exp env)
    (cases data-exp d-exp
      (a-data-exp (data-type val-constrs)
                  (foldl
                   (lambda (val-constr env) (create-val-constructor-proc val-constr data-type env))
                   env
                   val-constrs)))))


(define (create-val-constructor-proc val-constr data-type env)

  (define create-proc
    (lambda (types fresh-b-vars curr-exp)
      (if (null? types)
          curr-exp
          (create-proc
           (cdr types)
           (cdr fresh-b-vars)
           (lambda-exp (car fresh-b-vars) (car types) curr-exp)))))

  (define generate-fresh-b-vars
    (lambda (i acc)
      (if (zero? i)
          (reverse acc)
          (generate-fresh-b-vars (- i 1) (cons (string->symbol (string-append "x" (number->string i))) acc)))))
  
  (cases val-constr-exp val-constr
    (a-val-constr (val-constr-name types)
                  (let* ([fresh-b-vars (generate-fresh-b-vars (length types) '())]
                         [fresh-type-value (gen-fresh-type-value val-constr-name data-type fresh-b-vars)] 
                         [proc (create-proc (reverse types) (reverse fresh-b-vars) fresh-type-value)])
                    (extend-env val-constr-name (newref (a-thunk proc (empty-env))) env))))) ;; we don't need for value contructor env so put empty-env


(define gen-fresh-type-value
  (lambda (val-constr-name data-type b-vars)
    (let ([fresh-type-value (type-value-exp type-value-id val-constr-name b-vars data-type)])
          (begin
            (set! type-value-id (+ type-value-id 1))
            fresh-type-value))))
