/* @flow */

// This will require ./node_modules/B.js.flow
var B1 = require('B');
(B1.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

// This will require ./node_modules/B.js.flow
var B2 = require('B.js');
(B2.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var C1 = require('package_with_full_main');
(C1.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var D1 = require('package_with_partial_main');
(D1.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var E1 = require('package_with_no_package_json');
(E1.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var F1 = require('package_with_dir_main');
(F1.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

// This will require ./node_modules/B.js.flow
var B3 = require('B');
(B3.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

// This will require ./node_modules/B.js.flow
var B4 = require('B.js');
(B4.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var C2 = require('package_with_full_main');
(C2.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var D2 = require('package_with_partial_main');
(D2.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var E2 = require('package_with_no_package_json');
(E2.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean

var F2 = require('package_with_dir_main');
(F2.fun(): boolean); // Error either Implementation ~> boolean or Declaration ~> boolean
