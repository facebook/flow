// Arity is enforced bidirectionally
function foo4(x: [1, 2]): [1] { return x; } // error
function foo5(x: [1]): [1, 2] { return x; } // error

// The empty array literal is a 0-tuple
const foo: [] = [];
(foo: [1]); // error
