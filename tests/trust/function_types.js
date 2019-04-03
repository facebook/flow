//@flow

function f(x:$Trusted<number>): number { return x; }

var a: ($Trusted<number> => number) = f;
var b: (any => number) = a; //fail
var c: (number=> number) = a; // fails
var d: ($Trusted<number> => any) = a;
var e: any = a; //fail
var h: ($Trusted<number> => $Trusted<number>) = a;

function g(x: number): $Trusted<number> { return 42; }

var x: ($Trusted<number> => number) = g;
var y: (number => $Trusted<number>) = g;
var z: (any => $Trusted<number>) = y;
var w: (number => number) = y;
var u: (number => any) = y;
var v: any = y;
