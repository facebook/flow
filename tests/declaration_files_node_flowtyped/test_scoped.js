//@flow

// This will require ./node_modules/@flowtyped/foo__bar/index.js.flow
var A = require('@foo/bar');
(A.fun(): string); // Error number ~> string