// @flow

declare function f1(x: mixed): boolean;
declare function f3(x: mixed): boolean checks (x !== null);
declare function f4(x: mixed): boolean checks (x !== null);

function f7(x: mixed) checks { return x !== null }
var a0 = (x: mixed) => x !== null;
var a1 = (x: mixed) checks => x !== null;

(x) checks => x !== null;
