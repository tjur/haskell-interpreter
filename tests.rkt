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

    ;; function application
    (test-fun-app-1 "\\ (lst :: int-list) -> (42:lst) [1,2,3]" "[42, 1, 2, 3]")
    (test-fun-app-2 "\\(x :: int) -> \\(y :: int) -> \\(z :: int) -> (x + y) 1 2 (1/0)" "3")
    (test-fun-app-3 "(\\(x :: int) (y :: int) (z :: int) -> (x + y)) 1 2 (1/0)" "3")
    (test-fun-app-4 "\\(x :: int) -> 42 True" error)

    ;; simple variables with let
    (test-var-1 "let (x :: int) = 42 in x" "42")
    (test-var-2 "let (x :: int) = [] in 42" error)
    (test-var-3 "let (y :: int) = 3
                     (z :: int) = 1/0
                  in (2 * y + y)" "9")
    (test-var-4 "100 * x" error)

    ;; let
    (test-let-1 "let (f :: int) (x :: int) = 42 in (f (1/0))" "42")
    (test-let-2 "let (f :: int) (x :: int) (y :: int) = x*x + y in (f 2 3)" "7")
    (test-let-3 "let (add1 :: int -> int) = (+) 1 in (add1 10)" "11")
    (test-let-4
     "let (fact :: int) (n :: int) = if n == 0 then 1 else (n * (fact (n - 1))) in (fact 5)" "120")
    (test-let-5 "let (f :: int-list) (x :: unit) = y
                     (y :: int-list) = [1,2,3]
                  in (f ())" "[1, 2, 3]")
    (test-let-6 "let (even :: bool) (x :: int) = if x == 0 then True else (odd (x - 1))
                     (odd :: bool) (x :: int) = if x == 0 then False else (even(x - 1))
                  in (even 42)" "True")
    (test-let-7 "let (even :: bool) (x :: int) = if x == 0 then True else (odd (x - 1))
                     (odd :: bool) (x :: int) = if x == 0 then False else (even(x - 1))
                  in (odd 42)" "False")
    (test-let-8 "let (ones :: int-list) = 1:ones
                     -- comment
                     (take :: int-list) (lst :: int-list) (n :: int) =
                       if n == 0 then [] else ((head lst) : (take (tail lst) (n - 1)))
                  in (take ones 5)" "[1, 1, 1, 1, 1]")
    (test-let-9 "let (xs :: int-list) = 1:ys
                     (ys :: int-list) = 2:xs
                     -- comment
                     (take :: int-list) (lst :: int-list) (n :: int) =
                       if n == 0 then [] else ((head lst) : (take (tail lst) (n - 1)))
                  in (take xs 6)" "[1, 2, 1, 2, 1, 2]")
    (test-let-10 "let (f :: bool) (x :: int) = x in 42" error)
    (test-let-11 "let (x :: int -> (int -> int)) = (>=) in 42" error)
    
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

    (test-declaration-5
      "((!!) :: int) (x:xs :: int-list) (0 :: int) = x;
       ((!!) :: int) (x:xs :: int-list) (n :: int) = xs !! (n - 1);
      
       [0, 1, 2, 3, 4, 5] !! 4"
       "4")

    (test-declarations-6
     "(len :: int) ([] :: int-list) = 0;
      (len :: int) (x:(y:xs) :: int-list) = 2 + (len xs);
      (len :: int) (x:xs :: int-list) = 1 + (len xs);

      len [1, 2, 3, 4, 5, 10]"
     "6")

    ;; algebraic data types
    (test-data-1
     "data PairBin = Pair Bin Bin;
      data Bin = Zero | One;

      Pair Zero One"
     "Pair (Zero One)")

    (test-data-2
     "data Bin = Zero | One;
      data PairInt = Pair int int;

     (Bin-to-int :: int) (Zero :: Bin) = 0;
     (Bin-to-int :: int) (One :: Bin) = 1;

     Pair (Bin-to-int One) (Bin-to-int Zero)"
     "Pair (1 0)")

    (test-data-3
     "data Tree = Leaf | Node Tree int Tree;

     (sumTree :: int) (Leaf :: Tree) = 0;
     (sumTree :: int) (Node l x r :: Tree) = sumTree l + x + (sumTree r);

     sumTree (Node (Node (Node Leaf 1 Leaf) 3 Leaf) 10 (Node Leaf 5 Leaf))"
     "19")

    (test-data-4
     "data Tree = Leaf | Node Tree int Tree;

      let (f :: Tree) (x :: int) = (Node Leaf x Leaf)
       in (f 42)" "Node (Leaf 42 Leaf)")

    (test-data-5
     "data Tree = Leaf | Node Tree int Tree;

      let (f :: Tree2) (x :: int) = (Node Leaf x Leaf)
       in (f 42)" error)

    ))