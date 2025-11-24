record R { // OK
  a: number,
}

const x = R {a: 1}; // OK

declare const y: R;
match (y) {
  R {...} => {} // OK
}
