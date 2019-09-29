var a: [] = [];
var b: [] = [123]; // Error - arity mismatch
var c: [number] = []; // nope
var d: [number, string] = [123,'duck'];
var e: [number, string,] = [123,'duck'];
var f: [number, string] = [123, 456];

let tests = [
  function(t: [number]) {
    t[1]; // error, out of bounds
    t[-1]; // error, out of bounds
  },
  // toString's to an invalid index
  function(t: [number, string]) {
    t[0.5]; // error: index out of bounds
    t[0.0000000000000000000001]; // error: index ("1e-22") out of bounds
  },
  // very close to 1 toString's to a valid index
  function(t: [number, string]) {
    (t[1e0]: boolean); // error: boolean !~> string
    (t[0.9999999999999999999999999999999999999999999]: boolean); // error: boolean !~> string
  },
  // very close to 0 toString's to a valid index
  function(t: [number, string]) {
    (t[56e-13214125]: boolean); // error: boolean !~> number
  },
  // through a variable
  function (t: [number]) {
    const x = 0.5;
    t[x]; // error, not an integer
  },
];
