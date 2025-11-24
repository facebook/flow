record R { // ERROR
  a: number,
}

const x = R {a: 1}; // ERROR

declare const y: R;
match (y) {
  R {...} => {} // ERROR
  _ => {}
}
