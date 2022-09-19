//@flow

function f(x): void {}
const function_expr = function(x): void {};
const arrow_expr = (x) => {};
const deep_function_expr = {f: {g: {h: function(x): void {}}}};
const deep_arrow_expr = {f: {g: {h: (x) => void {}}}};
