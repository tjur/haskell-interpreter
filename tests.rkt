#lang eopl

(provide test-list)


(define test-list
  '(

    ;; simple const values
    (positive-const-num "11" "11")
    (negative-const-num "-33" "-33")
    (negative-const-num2 "-33-" error)

    ))