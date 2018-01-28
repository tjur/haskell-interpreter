#lang eopl

(require "datatypes.rkt")
(require "environments.rkt")
(require "store.rkt")
(require "type-checker.rkt")
(require (only-in racket/base
                  foldl reverse remove* void))

(provide process-data-exps add-type-to-check-list! initialize-types-to-check-list! get-val-constr-arg-types)


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
          (void) ;; Ok - every type exist
          (if (null? (cdr new-list))
              (eopl:error 'type-error "No such type: ~s" (type-to-external-form (car new-list)))
              (eopl:error 'type-error "No such types: ~s" new-list))))))
  

;; returns new env with value constructors as a procedures
(define process-data-exps
  (lambda (data-exps env)
    (begin
      (initialize-type-value-id!)
      (initialize-vctenv!)
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
    (a-val-constr (val-constr-name b-vars-types)
                  (let* ([fresh-b-vars (generate-fresh-b-vars (length b-vars-types) '())]
                         [fresh-type-value (create-type-value val-constr-name data-type fresh-b-vars b-vars-types)] 
                         [proc (create-proc (reverse b-vars-types) (reverse fresh-b-vars) fresh-type-value)])
                    (begin
                      (extend-vctenv val-constr-name b-vars-types)
                      (extend-env val-constr-name (newref (a-thunk proc (empty-env))) env)))))) ;; we don't need for value contructor env so put empty-env


(define create-type-value
  (lambda (val-constr-name data-type b-vars b-vars-types)
    (let ([type-value (type-value-exp val-constr-name b-vars b-vars-types data-type)])
      type-value)))


;;;;;;;;;;;;;;;;;;; value contructors arguments types environment ;;;;;;;;;;;;;;;;;;;

;; maps name of the value contructor to list of types of arguments it takes

(define-datatype val-constr-types-environment val-constr-types-environment?
  (empty-vctenv-record)
  (extended-vctenv-record
   (val-constr-name symbol?)
   (arg-types (list-of type?))
   (vctenv val-constr-types-environment?)))

(define vctenv 'uninitialized)
(define empty-vctenv '())

(define initialize-vctenv!
  (lambda ()
    (set! vctenv empty-vctenv)))

(define extend-vctenv
  (lambda (val-constr-name arg-types)
    (set! vctenv (cons (list val-constr-name arg-types) vctenv))))

(define apply-vctenv
  (lambda (vctenv val-constr-name)
    (if (null? vctenv)
      (eopl:error 'get-val-constr-arg-types "Given value constructor doesn't exists: ~s" val-constr-name)
      (let* ([res (car vctenv)]
            [name (list-ref res 0)]
            [types (list-ref res 1)]
            [saved-vctenv (cdr vctenv)])
        (if (eqv? val-constr-name name) 
            types
            (apply-vctenv saved-vctenv val-constr-name))))))

(define get-val-constr-arg-types
  (lambda (val-constr-name)
    (apply-vctenv vctenv val-constr-name)))
