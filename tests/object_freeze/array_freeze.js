/* @flow */

declare var x: Array<string>
var foo = Object.freeze(x)

const bar = Object.freeze([('12345': '12345')])
bar[0] = 1; // error

var bazz = [12345]
var bliffll = Object.freeze(['12345', ...bazz])
bliffll[0] = '12345'
bliffll[1] = 3456
bliffll[2]; // no error, because it isn't tuple
bliffl.toString = function() {}; // error

var yy: $ReadOnlyArray<number> = Object.freeze(['error'])

function g(y: any) {
  (Object.freeze([...y]): any); // ok

  let z = Object.freeze([...y]);
  z[1] = "bar"; // there is no frozen form of AnyT so this is "allowed"
}