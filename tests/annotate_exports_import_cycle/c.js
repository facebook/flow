// @flow

import { foo, baz } from "./lib";

const a = require("./a");

module.exports = [
  0 < 1 ? foo() : baz()
];
