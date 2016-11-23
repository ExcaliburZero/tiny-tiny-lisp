# Tiny Tiny Lisp
Tiny Tiny Lisp is a small implementation of an interpreter for a small lisp dialect.

```
% (+ 1 2)
3.0
% (* 2 (- 5 2) 1)
6.0
% (/ 2 1 1)
2.0
```

## Usage
To run the interpreter for Tiny Tiny Lisp, run the following command. You can then input a valid value and press enter and the interpreter will evaluate it.

```bash
$ tiny-tiny-lisp
% (- 2 1)
1.0
% :q
```

In order to quit out of the interpreter, you can enter `:q`.

## Installation
To install the interpreter for Tiny Tiny Lisp, clone the GitHub repository and install it using Stack.

```bash
$ git clone https://github.com/ExcaliburZero/tiny-tiny-lisp.git
$ cd tiny-tiny-lisp
$ stack install
```

## Grammar
```
Value --> ( Expr ), Lit
Expr --> Func [Value]
Func --> Op
Op --> +, -, *, /
Lit --> Number
Number --> Decimal, Integer
Decimal --> [Digit] . [Digit]
Integer --> [Digit]
```
