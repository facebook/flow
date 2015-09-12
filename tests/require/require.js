/* @flow */

function takesANumber(num: number): void {}
function takesAString(str: string): void {}

// @providesModule
var A = require("A");
takesANumber(A.numberValue);
takesAString(A.numberValue);

// File path
var B = require("./B");
takesANumber(B.numberValue);
takesAString(B.numberValue);

// C.js exists, but not as a providesModule
require("C");

// @providesModule D exists, but not as a filename
require("./D");

// E exports an object with a numVal property
var E = require('./E');
var e_1: number = E.numberValue;
E.stringValue; // Error: The E exports obj has no 'stringValue' property
