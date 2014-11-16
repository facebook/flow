//function foo<U> (x: Array<U> | U): Array<U> { return []; }

//var x1:number = foo(0)[0];
//var x2:string = foo([0])[0];

function bar(x: Document | string): void { }
bar(0);

class C { }
class D { }
var E = C || D;
var c:C = new E();
function qux(e: E | string) { }
qux(new C);

declare class F {
    foo(x: number):void;
    foo(x: string):void;
}
function corge(b) {
    var x = b ? "" : 0;
    new F().foo(x);
}
