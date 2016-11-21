# Tiny Tiny Lisp
Tiny Tiny Lisp is a small implementation of an interpreter for a small lisp dialect.

## Grammar
```
Value --> ( Expr )
Expr --> Func [Value]
Func --> Op
Op --> +, -, *, /
```
