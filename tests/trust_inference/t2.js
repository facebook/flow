//@flow

function h(x: $Trusted<number>): number {
  return 42 as any;
}

function g(x: number) {
  return h(x);
}

function f(x: number): $Trusted<number> {
  return g(x); // Fails.
}

f(42);
f(42 as any);

var x: number => number = function (x: number): number {
  return 42 as any;
};
var y: number => $Trusted<number> = x;

var a: ($Trusted<number>) => $Trusted<number> = function (
  x: $Trusted<number>,
): number {
  return x;
};
var b: number => number = a;
b as any;

declare var j: number;

var o1 = {x: j};
var o2: {x: $Trusted<number>} = o1;
var o3: {x: number} = o1;
o3.x = 'a' as any;
