// @flow

// This test ensures that no imports are added since there is an error

import { foo, bar } from "./lib";
import { error_foo, error_bar } from "./error-lib";

const b = require("./error-b");

module.exports = [
  0 < 1 ? foo() : error_foo(),
  0 < 1 ? bar() : error_bar(),
];
