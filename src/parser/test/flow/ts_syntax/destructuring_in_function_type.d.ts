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
type A = ({ param1, param2 }: Type) => ReturnType;
type B = ({ x: x1, y: y1 }: Type) => ReturnType;
type C2 = ([elem1, elem2]: TupleType) => ReturnType;
type D = ([, value]: TupleType) => ReturnType;
type E = ({a}?: T) => R;
type F = ([a]?: T) => R;
type G = ({a, ...b}: T) => R;
type H = ({a: {b}}: T) => R;
type I2 = ({a}: T, {b}: U) => R;
type J = ({a}: T, x: U) => R;
type K = (...{a}: T) => R;
type L = (...[a, b]: T) => R;
