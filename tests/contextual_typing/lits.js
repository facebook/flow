//@flow

let h = []; // err
let x: Array<number> = []
let y: Array<Array<number>> = [[]];
let z: { a?: ?Array<number> } = { a: [] }
let w: { ... } = { a: [] } // err
let u: [number] = []; // err
let v: $ReadOnlyArray<number> = [];

declare function f(Array<number>): void;
[]; //err
f([]);

[].concat([]);

function array_default_1({a = [] /* ok */}: {a?: Array<string>}): void {
  (a: Array<string>); // ok
}
function array_default_2([a = [] /* ok */]: [Array<string> | void]): void {
  (a: Array<string>); // ok
}
function array_default_3({a = [] /* ok */, b: [b = []] /* ok */}: {a?: Array<string>, b: [Array<string> | void]}): void {
  (a: Array<string>); // ok
  (b: Array<string>); // ok
}
{
  const {a1 = [] /* ok */}: {a1?: Array<string>} = (1: any);
  (a1: Array<string>); // ok
  const [a2 = [] /* ok */]: [Array<string> | void] = (1: any);
  (a2: Array<string>); // ok
  const {a3 = [] /* ok */, b: [b3 = []] /* ok */}: {a3?: Array<string>, b: [Array<string> | void]} = (1: any);
  (a3: Array<string>); // ok
  (b3: Array<string>); // ok
}
