// No errors are expected in this file.

[...[1,2,3]];
const a: Array<number> = [4,5,6];
[...a];
f(...a);
[...a.map(x => x + 1)];
f(...a.map(x => x + 1));
const b: [number, string] = [42, "foo"];
[...b];
f(...b);
f.apply(null, b);
f.bind(null, ...b);

function f(...args) {}

declare var compose: $Compose;
compose(...[x => x, x => x]);
