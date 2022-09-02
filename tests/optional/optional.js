function bar(x?: number, y?: mixed) { x * 0; }
bar(0);

var foo:(x?:number)=>void = bar;
foo();

function qux(x: number = "hello", ...y: [string, mixed]):string { foo(x); return y[0]; } // error: string ~> number

qux(0,0); // Error, number ~> string
qux(0,...[42, ""]); // Error, number ~> string
qux(0,...["",42]); // No error

module.exports = qux;
