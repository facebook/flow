//@flow

declare var c: boolean;

var x = c ? 42 : "a'";

var y = 42;

if (typeof x === 'string') {
  x as string;
  y = x; // error: string ~> number
}

y as number; // ok: y always has type number

var z;
z = 42;
z as number;
z = 'a';
z as string;

var a1 = 42;
a1 as number;
var a2 = a1;
a2;
a2 = 'hello world';
a1 as number;
a2 as number;

function f(x: number): string {
  return x;
}

f() as string;

var ya: number;
ya as number;
ya = 'a';

function bar(y: {[key: string]: mixed}): string {
  return y.foo;
}

function foo(x: {[key: string]: mixed}) {
  bar(x);
}

var x2 = f(42);
x2 as string;
x2 = y;

var p11 = 0;
var y11 = [1];
if (y11[p11 as empty]) {
}
