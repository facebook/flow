// arg/param type mismatch on arg 0
function g0(y:string) { }
function f0(x: number) { g0(x) }
f0(0);

// ...on arg n
function g1(a:string, b:string) { }
function f1(x: string, y: number) { g1(x, y) }
f1("hey", 0);

// h/o call with function expr
function g2(ylam: (s:string) => number) { }
function f2(xlam: (s:string) => number) { g2(xlam) }
f2(function(x) { return x * x });

// h/o call with function def
function g3(ylam: (s:string) => number) { }
function f3(xlam: (s:string) => number) { g3(xlam) }
function double(n: string) { return n * 2 }
f3(double);
