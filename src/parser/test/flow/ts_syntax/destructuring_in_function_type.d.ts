declare function f({a, b}: T): R;
declare function g({a: b}: T): R;
declare function h([a, b]: T): R;
declare function nested({a: {b}}: T): R;
declare function rest({a, ...b}: T): R;
declare function optObj({a}?: T): R;
declare function optArr([a]?: T): R;
declare class C { m({a}: T): R; }
declare class D { m({a}?: T): R; }
interface I { m([a, b]: T): R; }
declare function restObj(...{a}: T): R;
declare function restArr(...[a, b]: T): R;
