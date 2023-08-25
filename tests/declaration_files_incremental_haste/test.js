/* @flow */

var Implicit = require('ImplicitProvidesModule');
(Implicit.fun(): boolean); // Error: Either Implementation ~> boolean or Declaration ~> boolean
