//@flow

// This will require ./node_modules/@flowtyped/foo/index.js.flow
var A = require('foo');
(A.fun(): string); // Error number ~> string