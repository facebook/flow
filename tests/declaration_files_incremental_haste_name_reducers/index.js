/* @flow */

var A = require('A');
A.foo() as boolean; // Error: Either AImplementation ~> boolean or ADefinition ~> boolean

var test = require('test');
test.foo() as boolean; // Error: Either TestImplementation ~> boolean or TestDefinition ~> boolean
