var x: null = null;
var o = { p: 0, ...x };
(o.p: empty); // error: number ~> empty
