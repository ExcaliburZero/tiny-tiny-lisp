# Tiny Tiny Lisp
Tiny Tiny Lisp is a small implementation of an interpreter for a small lisp dialect.

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
