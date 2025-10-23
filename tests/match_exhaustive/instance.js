declare class C {
  b: boolean;
}

declare const x: C;

match (x) { // OK
  C {...} => {}
}

match (x) { // OK
  C {b: _, ...} => {}
}

match (x) { // ERROR: missing with `b: false` case
  C {b: true, ...} => {}
}

match (x) { // OK
  C {b: true, ...} => {}
  C {b: false, ...} => {}
}
