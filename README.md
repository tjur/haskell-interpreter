# haskell-interpreter
We present the interpreter of JuRa - language implementing small subset of Haskell with call by need evaluator, algebraic data types
and pattern matching. The interpreter is written in Racket.

## Syntax of the language
Syntax of JuRa is very similar to Haskell.
We can split every program expressions to 3 different types:
1. Declarations (with pattern matching)
2. Algebraic data types definitions (not mutually recursive and without polymorphism)
3. Expressions

Each of these must be seperated in program by `;` sign (but it shouldn't be added after the last line of a program!).

### Types
JuRa is type annotated. Programmer is required to write down type signatures for bounded variables
in lambda and let expressions. It is also obligatory to add result types for every function/variable defined
in let expression.

Examples:
```
\(x :: int) (xs :: int-list) -> (x:xs)

let (f :: bool) (xs :: int-list) (y :: int) = ((head xs) == y)
    (lst :: int-list) = [1,2,3]
    (add1 :: int -> int) = (+) 1
  in (f lst 1)
```
There are 4 basic types: `int`, `bool`, `unit`, `int-list`. For writing types for functions there is `->` sign (e.g., `int -> (bool -> int-list)`). Apart from that programmers can define and use their own algebraic data types.

## Project
The project consists of the following files:
* `parser.rkt` - scanner and parser of the language
* `interp.rkt` - evaluator for the language
* `datatypes.rkt` - definitions of data types used by the interpreter
* `data-expression.rkt` - code for handling algebraic data types
* `declarations-translator.rkt` - code for handling global declarations with pattern matching
* `basic-procedures.rkt` - code for handling basic procedures for numbers and lists (e.g., +, ==, >=, mod, head, tail)
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

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Authors

* **[Tomasz Jurkiewicz](https://github.com/tjur)**
* **[Dominik Rabij](https://github.com/jamone-)**
