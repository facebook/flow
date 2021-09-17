//@flow

declare var c: boolean

var x = c ? 42 : "a'";

var y = 42

if (typeof x === 'string') {
  (x: string);
  y = x;
}

(y: number);

var z;
z = 42;
(z: number);
z = 'a';
(z: string);

var a1 = 42;
(a1: number);
var a2 = a1;
a2;
a2 = "hello world";
(a1: number);
(a2: number);


function f(x: number): string {
  return x;
}

(f(): string);

var ya: number;
(ya: number)
ya = "a"

function bar(y): string {
  return y.foo;
}

function foo(x: {[key: string]: mixed}) {
  bar(x);
}

var x2 = f(42);
(x2: string);
x2 = y;
