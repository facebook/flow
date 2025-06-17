declare const x: empty;

match (x) {} // OK

match (x) {
  1 => {} // ERROR
  _ => {} // ERROR
}
