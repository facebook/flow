`main.js` defines a `Result` type: a tagged union of a successful result
(carrying a numeric `value`) and a failed one (carrying an `error` message).

Implement `collectValues` as an arrow function with an expression body (no
braces) and type `(results: Array<Result>) => Array<number>`. It first narrows
the array to the successful results, then reads each `value`, preserving order.
