/* @flow */

var min = require('d3/min.js');
var corge = require('qux/corge');
var SomeOtherModule = require('SomeOtherModule'); // error

(min.fun(): string);
(corge.fun(): string);
