var vars = require("./cycle_B.js")

function f(x) {
  if (x); /* sketchy because of uses */
}

const resNull: void = f(null);
const resB: void = f(vars.b);

module.exports = {resA: resNull, resB: resB};
