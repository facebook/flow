//@flow

let x: {||} = 3;
let y: {||} = '';
let z: {||} = true;

let c: {||} = new (class {})();
let f: {||} = () => {};

declare var u: {||} | {};
u as {||};

declare var v: number | {};
v as {||};

class A {}

0 as $Exact<number>;
'' as $Exact<string>;
true as $Exact<boolean>;
new A() as $Exact<A>;
(() => {}) as $Exact<() => {}>;

let foo = () => {};
(() => {}) as $Exact<typeof foo>;
let bar = new A();
new A() as $Exact<typeof bar>;
