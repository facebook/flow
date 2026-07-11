`main.js` defines an `Expr` type: a recursive expression tree whose nodes (`num`, `add`, `mul`, `neg`) are tuples tagged by their first element.

Write a Flow function `evaluateExpr(expr: Expr): number` that recursively evaluates the expression tree, using `match` expressions with tuple destructuring.
