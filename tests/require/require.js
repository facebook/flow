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
