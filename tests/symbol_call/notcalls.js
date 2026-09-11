// The two callees are read from the shape of the call. A spelling one step away
// from either of them is not one of them, so the call below is left alone and
// returns what its callee says it returns.

// `new Symbol()` is a runtime error, and it is not the call that mints.
const constructed = new Symbol();
const fromConstructed = {[constructed]: 1}; // ERROR: an instance is not a key

// An optional call is a different expression, and one that can return `void`.
const optional = Symbol?.();
const fromOptional = {[optional]: 1}; // ERROR: `symbol` is not a key

// The registry lookup is read as the property `for` written as a name, so the
// bracketed spelling is an ordinary element access, which the class does not
// answer at all.
const bracketed = Symbol['for']('a key'); // ERROR: no index signature on `Symbol`

// Another static of `Symbol` is not the registry lookup.
declare const someSymbol: symbol;
const keyOf = Symbol.keyFor(someSymbol);
const fromKeyFor = {[keyOf]: 1}; // ERROR: `?string` is not a key

// `for` on something else is not the registry lookup either.
declare const registry: {for: (key: string) => symbol};
const elsewhere = registry.for('a key');
const fromElsewhere = {[elsewhere]: 1}; // ERROR: `symbol` is not a key
