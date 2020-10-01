// @flow

const dep = require("./dep1");
var b: boolean = dep.INSPECT_MAX_BYTES; // error, number ~/> boolean
