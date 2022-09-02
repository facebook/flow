var vars = require("./cycle_B.js")

function f(x: number | null) {
  if (x);
}

const resNull: void = f(null);
const resB: void = f(vars.b);

module.exports = {resA: resNull, resB: resB};
