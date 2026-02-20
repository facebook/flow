// @flow

// Test: Flow comment syntax with various types before comment close.
// These test that multi-char tokens don't eat into */ comment close.

// Normal types in comment syntax work fine
/*:: declare var numInComment: number */
(numInComment: number); // ok

// Type annotation using comment syntax with various token boundaries
function f(x/*: number */)/*: number */ { return x; }

// Nullable type in comment syntax
function g(x/*: ?number */)/*: ?number */ { return x; }

// Union type with | right before */
/*:: type U = string | number */
var u: U = 42;
