var a: [] = [];
var b: [] = [123]; // Error - arity mismatch
var c: [number] = []; // nope
var d: [number, string] = [123,'duck'];
var e: [number, string,] = [123,'duck'];
var f: [number, string] = [123, 456];
(f[0.5]: number);
(f[0.9999999999999999999999999999999999999999999]: string);
