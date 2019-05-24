//@flow

// This will require ./node_modules/@flowtyped/bar/code.js.flow
var A = require('bar');
(A.fun(): string); // Error number ~> string
