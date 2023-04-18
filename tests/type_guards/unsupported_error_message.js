type T = (x: any) => x is number; // error unsupported
class C { m(x: any): x is number { return true; } }; // error unsupported
declare class D { m(x: any): x is number }; // error unsupported
declare function f(x: any): x is number; // error unsupported
function g(x: any): x is number { return true }; // error unsupported
function h<T>(cb: (x: any) => x is T): T { throw "unsupported" }; // error unsupported
