/* @flow */

// This will require ./node_modules/test/.A.js
var B1 = require("test/.A");
(B1.fun(): string); // Error number ~> string

// This will require ./node_modules/test/.js
var B2 = require("test/.js");
(B2.fun(): number); // Error string ~> number
