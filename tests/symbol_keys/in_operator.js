// A `unique symbol` (and a generic `symbol`) is a valid left-hand side of the
// `in` operator, matching JavaScript/TypeScript. Regression: the LHS was
// required to be a string or number, rejecting symbol keys.
declare const a: unique symbol;
declare const o: {[a]: number, x: boolean};

(a in o) as boolean; // OK: unique symbol LHS
"x" in o; // OK: string LHS still works

declare const gen: symbol;
(gen in o) as boolean; // OK: generic symbol LHS

// A generic bounded by a `unique symbol` is also a valid LHS.
function f<K extends typeof a>(k: K): boolean {
  return k in o; // OK
}
