declare const a: $ReactDeepReadOnly<{x: {y: number}}>;
a.x.y = 42; // error
a.x = 42; // error

declare const x: $ReactDeepReadOnly<{x: {a: number}, y: Array<{w: {v: number}}>}>;
const y = x.y;
const z = y[0]
const w = z.w;
const v = w.v;
(v: empty); //error
w.v = 0; // error
x.x.a = 42; // error
x.x.y = []; // error
x.x = (42: any);

declare const rr: $ReactDeepReadOnly<{current: number}>;
rr.current = 42; // ok
