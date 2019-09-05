//@flow

// This will require ./node_modules/baz/index.js.flow
var A = require('baz');
(A.fun(): number); // Error string ~> number
