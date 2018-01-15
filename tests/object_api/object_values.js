/* @flow */

declare var o1: {[string]: boolean};
for (const v of Object.values(o1)) {
  (v: boolean);
  (v: string) // error: boolean ~> string
}

declare var o2: {[string]: 1 | 2};
for (const v of Object.values(o2)) {
  (v: 1 | 2);
  (v: number);
}

declare var o3: {a: 1, b: 2};
for (const v of Object.values(o3)) {
  if (v !== 1) {
    (v: 2);
    (v: 3); // error: 2 ~> 3
  }
  if (v !== 2) {
    (v: 1);
    (v: 3); // error: 1 ~> 3
  }
}
