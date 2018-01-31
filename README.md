# haskell-interpreter
We present the interpreter of JuRa - language implementing small subset of Haskell with call by need evaluator, algebraic data types and pattern matching. The interpreter is written in Racket.

## Syntax of the language
Syntax of JuRa is very similar to Haskell.
We can split every program expressions to 3 different types:
### 1. Algebraic data types definitions
JuRa language allows programmers to define their own simple algebraic data types. Data types can't be mutually recursive and don't use polymorphism. Syntax is the same as in Haskell:

```haskell
data Tree = Empty | Leaf int | Node Tree int Tree
```

```haskell
data PairInt = Pair int int
```

```haskell
data Bin = Zero | One
```

```haskell
data Maybe = Nothing | Just int
```

All data types are extracted from the program's source code before running any of the expression and therefore they can be later used for type annotations in program expressions.

### 2. Declarations
Using declarations programmer can extend the global environment with new definitions of variables or functions. Declarations can be placed in global scope only (programmer cannot create new declaration in some expression body). Declarations don't return any value, they are just adding new informations to known the environment.

Declaration begins with a variable/function name followed by the type/result type of the defined variable/function which are surrounded by brackets. Every argument of the declared function has to provide its type too.

Simple declaration examples:

```haskell
(ones :: int-list) = 1:ones
```

```haskell
(f :: int) (x :: int) (y :: int) = x + y
```

```haskell
(add1 :: int -> int) = (+) 1
```

Declarations support pattern matching on arguments, such as matching to:
- a constant of type `int`, `int-list`, `bool`, `unit`,
- some list structure (using `:` or `[]`),
- known datatype value constructor,
- a new variable.

Examples of declarations with pattern matching:

```haskell
(and :: bool) (True :: bool) (True :: bool) = True;
(and :: bool) (_ :: bool) (_ :: bool) = False

-- note that _ isn't any special variable it's just normal variable name
-- and when a variable occurs again in a definition it overrides previous occurences
```

```haskell
(fact :: int) (0 :: int) = 1;
(fact :: int) (n :: int) = n * (fact (n - 1))
```

```haskell
(take :: int-list) (x:xs :: int-list) (0 :: int) = [];
(take :: int-list) (x:xs :: int-list) (n :: int) = x:(take xs (n - 1))
```

```haskell
(length :: int) ([] :: int-list) = 0;
(length :: int) (_:(_:xs) :: int-list) = 2 + (length xs);
(length :: int) (_:xs) :: int-list) = 1 + (length xs)
```

```haskell
(pick :: int) ([0, x, y] :: int-list) = x;
(pick :: int) ([1, x, y] :: int-list) = y

-- or equivalently:

(pick :: int) (0:(x:(y:[])) :: int-list) = x;
(pick :: int) (1:(x:(y:[])) :: int-list) = y
```

```haskell
data Tree = Leaf | Node Tree int Tree;

(sumTree :: int) (Leaf :: Tree) = 0;
(sumTree :: int) (Node left x right :: Tree) = sumTree left + x + (sumTree right)
```

Declarations give possibility to define programmer's own infix operators, just surround the operator by additional brackets; for instance:

```haskell
((++) :: int-list) ([] :: int-list) (ys :: int-list) = ys;
((++) :: int-list) (x:xs :: int-list) (ys :: int-list) = x:(xs ++ ys)
```

```haskell
((!!) :: int) (x:xs :: int-list) (0 :: int) = x;
((!!) :: int) (x:xs :: int-list) (n :: int) = xs !! (n - 1)
```

Note that when arguments patterns in declarations of a function are of the same type (for example in two declarations, at the first argument position, pattern matches head and body of a list) and are using variable names, they have to have exactly the same names at the same positions:

```haskell
(f :: int) (0:xs :: int-list) = 0;
(f :: int) (x:ys :: int-list) = (f ys)    -- won't work: ys is undefined

-- has to be changed to:

(f :: int) (0:xs :: int-list) = 0;
(f :: int) (x:xs :: int-list) = (f xs)
```

Declarations are mutually recursive:

```haskell
(even :: bool) (0 :: int) = True;
(even :: bool) (n :: int) = odd (n - 1);

(odd :: bool) (0 :: int) = False;
(odd :: bool) (n :: int) = even (n - 1)
```

All declarations are extracted from the program's source code before running any of the expression. So for example an expression at the middle of a source code can use declaration of a variable/function that can occur later in the source code.

### 3. Expressions
Expressions are evaluated by the interpreter and their values are returned. Program can contain many expression - they will be evaluated separately in order of appearance in a program and results will be printed.

Expressions can be built using:
* value expression - it can be int, bool, unit or int-list
```haskell
42;
True;
();
[1,2,3]
```
* variable expression
```haskell
x;
hello2;
some-dashed-variable
```
* if expression
```haskell
if <bool-exp> then <exp> else <exp>

-- for instance:

if 2 == 3 then 42 else 27
```
* lambda expression
```haskell
\(x :: int) (xs :: int-list) -> ((x+1):xs);
\(x :: int) -> \(y :: int) -> (x + y)
```
* call expression
```haskell
\(x :: int) -> x 42;
head [1,2,3];
(*) 2 3;
2 * 3;
mod 10 3;
10 `mod` 3
```
* let expression
```haskell
let (id :: int) (x :: int) = x in (id 42);

let (even :: bool) (x :: int) = if x == 0 then True else (odd (x - 1))
    (odd :: bool) (x :: int) = if x == 0 then False else (even(x - 1))
 in (even 42);

let (xs :: int-list) = 1:ys
    (ys :: int-list) = 2:xs
    (take :: int-list) (lst :: int-list) (n :: int) =
      if n == 0 then [] else ((head lst) : (take (tail lst) (n - 1)))
 in (take xs 6)
```


All data definitions, declarations and expressions must be seperated in program by `;` sign (but it shouldn't be added after the last line of a program!).

### Types
JuRa is type annotated. Programmer is required to write down type signatures for bounded variables in lambda and let expressions. It is also obligatory to add result types for every function/variable defined in let expression.

Examples:
```haskell
\(x :: int) (xs :: int-list) -> (x:xs)
```

```haskell
let (f :: bool) (xs :: int-list) (y :: int) = ((head xs) == y)
    (lst :: int-list) = [1,2,3]
    (add1 :: int -> int) = (+) 1
  in (f lst 1)
```
There are 4 basic types: `int`, `bool`, `unit`, `int-list`. For writing types for functions there is `->` sign (e.g., `int -> (bool -> int-list)`). Apart from that programmers can define and use their own algebraic data types.

### Provided functions
JuRa provides basic functions such as:
 - `int` operators:
     - of type `int -> int -> int`: `+`, `-`, `*`, `/`, `mod`
     - of type `int -> int -> bool`: `<`, `<=`, `>`, `>=`
 - `lists` procedures:
     - `head :: int-list -> int`
     - `tail :: int-list -> int-list`
     - `empty :: int-list -> bool`
 - common compare operators (compare by value on `int`, `bool`, `unit`; other types are compared by reference):
     - `== :: any -> any -> bool`
     - `!= :: any -> any -> bool`

## Project
The project consists of the following files:
* `parser.rkt` - scanner and parser of the language
* `interp.rkt` - evaluator for the language
* `datatypes.rkt` - definitions of data types used by the interpreter
* `data-expression.rkt` - code for handling algebraic data types
* `declarations-translator.rkt` - code for handling global declarations with pattern matching
* `basic-procedures.rkt` - code for handling basic procedures for numbers and lists (e.g., `+`, `==`, `>=`, `mod`, `head`, `tail`)
* `environments.rkt`
* `store.rkt`
* `type-checker.rkt`
* `tests.rkt`
* `run-tests.rkt`
* `run.rkt` - runs given file with a program written in JuRa

Additionally there is a catalog `example-programs` with exemplary programs in JuRa.

## Running tests
To run tests from tests.rkt run command: `racket run-tests.rkt`

## Running a program
To run a file with program just run command: `racket run.rkt <path-to-file-with-program>`

## Possible future improvements
* Polymorphism
* Type inference
* Improve the quality of printed errors
* Change associativity of `:` operator to right
* Add priority to arithmetic operators
* Add bool operators
* Add pattern matching to let expression
* Change type annotations for declarations to be less verbose

## License
This project is licensed under the MIT License - see the [LICENSE](https://github.com/tjur/haskell-interpreter/blob/master/LICENSE) file for details.

## Authors

* **[Tomasz Jurkiewicz](https://github.com/tjur)**
* **[Dominik Rabij](https://github.com/jamone-)**
