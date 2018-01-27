#lang eopl

(provide test-list)


(define test-list
  '(

    ;; simple const values
    (positive-const-num "11" "11")
    (negative-const-num "-33" "-33")
    (true-const-bool "True" "True")
    (false-const-bool "False" "False")
    (unit "()" "()")

    ;; number procedures
    (test-num-proc-1 "1 + 2 + 3 + 4" "10")
    (test-num-proc-2 "(6 * 5) + 55 - 1 / 2" "42")
    (test-num-proc-3 "1 / 0" error)
    (test-num-proc-4 "5 == 5" "True")
    (test-num-proc-5 "6 == 7" "False")
    (test-num-proc-6 "2 != 3" "True")
    (test-num-proc-7 "7 > 8" "False")
    (test-num-proc-8 "8 >= 7" "True")
    (test-num-proc-9 "2 < 3" "True")
    (test-num-proc-10 "4 <= 3" "False")
    (test-num-proc-11 "mod 20 5" "0")
    (test-num-proc-12 "7 `mod` 3" "1")

    ;; lists
    (test-list-1 "[]" "[]")
    (test-list-2 "[1,2,3]" "[1, 2, 3]")
    (test-list-3 "[1, True, 3]" error)
    (test-list-4 "[2+3, 4*5 - 1, 16/4]" "[5, 19, 4]")
    (test-list-5 "1 : (2 : (3 : (4 : [])))" "[1, 2, 3, 4]")
    (test-list-6 "head []" error)
    (test-list-7 "head [5,6,7]" "5")
    (test-list-8 "tail []" error)
    (test-list-9 "tail [5,6,7]" "[6, 7]")
    (test-list-10 "empty []" "True")
    (test-list-11 "empty [5,6,7]" "False")

    ;; simple unbound variables
    (test-unbound-var-1 "foo" error)
    (test-unbound-var-2 "100 * x" error)

    ))