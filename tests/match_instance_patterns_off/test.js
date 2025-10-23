class C {x: number}

declare const x: C;

match (x) { // OK
  C {...} => {} // ERROR
  _ => {}
}

match (x) {} // ERROR: pattern suggested is `{...}`, not `C {...}`
