// @flow

var Parent = require('./Parent');

// Hops through destructuring
let ParentFoo;
({ParentFoo} = Parent);
// ^
ParentFoo; // Points to lval in line above this
// ^
