function foo(x: number) { return [x, x > 0, "number " + x]; }

var [n, b, s] = foo(42);
n * s.length;
