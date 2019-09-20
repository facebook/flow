/* @flow */

// This will require ./.A.js.flow
var A1 = require("./.A");
(A1.fun(): string); // Error number ~> string

// This will require ./.A.js.flow
var A2 = require("./.A.js");
(A2.fun(): string); // Error number ~> string

// This will require ./.js.flow
var A3 = require("./.js");
(A3.fun(): string); // Error number ~> string

// This will require ./index.js
var A4 = require("./.");
(A4.fun(): number); // Error string ~> number

var CJS = require("./.CJS.js");
(CJS: string);
(CJS: number); //
