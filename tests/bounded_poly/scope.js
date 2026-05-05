function foo<X, Y extends X>(x:X, y:Y):void { }
foo(0, "");

function bar<X extends number, Y extends X>(x:X, y:Y): number { return y*0; }
