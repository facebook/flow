/* @flow */

var min = require('d3/min.js');
var corge = require('qux/corge');

(min.fun(): boolean); // Error: Either Implementation ~> boolean or Declaration ~> boolean
(corge.fun(): boolean); // Error: Either Implementation ~> boolean or Declaration ~> boolean
