// @flow

import { foo, baz } from "./lib";

const c = require("./c");

module.exports = [foo(), baz()];
