var o = { x: 5, y: "jello" };
var z = o.z;
var export_o: { x: number; } = o;

function f(u: number, v?: mixed): number { return u; }
var export_f: (u: number) => number = f;

//exports = export_o;
module.exports = export_f;
