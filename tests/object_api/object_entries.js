/* @flow */

declare var o1: {[string]: boolean};
for (const [k, v] of Object.entries(o1)) {
  (k: string);
  (v: boolean);
  (v: string) // error: boolean ~> string
}

declare var o2: {['a' | 'b']: 1 | 2};
for (const [k, v] of Object.entries(o2)) {
  (k: 'a' | 'b');
  (k: string);
  (v: 1 | 2);
  (v: number);
}

declare var o3: {a: 1, b: 2};
for (const [k, v] of Object.entries(o3)) {
  // hopefully these refinements will work one day
  if (k === 'a') {
    (v: 1 | 2);
    (v: 1); // error: 1 ~> 2
  }
  if (k === 'b') {
    (v: 1 | 2);
    (v: 2); // error: 2 ~> 1
  }
}
