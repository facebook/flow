// @flow

import { foo, baz } from "./lib";
import { error_foo, error_baz } from "./error-lib";

const a = require("./error-a");

module.exports = [
  0 < 1 ? foo() : error_foo(),
  0 < 1 ? baz() : error_baz(),
];
