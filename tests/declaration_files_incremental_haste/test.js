/* @flow */

var Implicit = require('ImplicitProvidesModule');
Implicit.fun() as boolean; // Error: Either Implementation ~> boolean or Declaration ~> boolean
