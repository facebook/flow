//@flow

let h = []; // err
let x: Array<number> = []
let y: Array<Array<number>> = [[]];
let z: { a?: ?Array<number> } = { a: [] }
let w: { ... } = { a: [] }
let u: [number] = []; // err

declare function f(Array<number>): void;
[]; //err
f([]);

[].concat([]); // err, but no divergence
