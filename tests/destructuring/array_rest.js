let xs = [0, "", true];
let [a, ...ys] = xs;
let [b, ...zs] = ys;
let c = zs[0]; // retain tuple info
let d = zs[1]; // run off the end

a as void; // error: number ~> void
b as void; // error: string ~> void
c as void; // error: boolean ~> void
d as void; // error: number|string|boolean ~> void

let [...e] = 0;

declare var arr :  [number|string];
[...arr] = [1]; // okay
[...arr] = [true]; // error boolean ~> number|string
