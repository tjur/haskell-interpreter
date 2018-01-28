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

    ;; if
    (test-if-1 "if 2+3==5 then 2*3 else (1/0)" "6")
    (test-if-2 "if 5 > 7 then 1/0 else 3+1" "4")
    (test-if-3 "if empty [42] then 1 else [2,3]" error)

    ;; simple variables with let
    (test-var-1 "let (x :: int) = 42 in x" "42")
    (test-var-2 "let (x :: int) = [] in 42" error)
    (test-var-3 "let (y :: int) = 3
                     (z :: int) = 1/0
                  in (2 * y + y)" "9")
    (test-var-4 "100 * x" error)

    ;; declarations
    (test-declaration-1
      "(id :: int) (x :: int) = x;
       id 420"
       "420")

    (test-declaration-2
      "(tribonacci :: int) (0 :: int) = 0;
       (tribonacci :: int) (1 :: int) = 1;
       (tribonacci :: int) (2 :: int) = 1;
       (tribonacci :: int) (n :: int) = tribonacci (n - 1) + (tribonacci (n - 2)) + (tribonacci (n - 3));

       tribonacci 7"
       "24")

    (test-declaration-3
      "(take :: int) (x:xs :: int-list) (0 :: int) = x;
       (take :: int) (x:xs :: int-list) (n :: int) = take xs (n - 1);

       take [0, 1, 2, 3, 4, 5] 4"
       "4")

    (test-declaration-4
      "(even :: bool) (0 :: int) = True;
       (even :: bool) (n :: int) = odd (n - 1);

       (odd :: bool) (0 :: int) = False;
       (odd :: bool) (n :: int) = even (n - 1);

       even 27"
       "False")

    ))