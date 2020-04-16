// @flow

import { foo, bar } from "./lib";

const b = require("./b");

module.exports = [foo(), bar()];
