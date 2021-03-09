const a = require("./cycle_A.js");

const b = {a: a, b: 0};

module.exports = b;
