function test1<S, T>(x: { [K in keyof S]: S[K] }): { [K in keyof T]: T[K] } {
  return x; // error S incompatible with T
}

function test2<S, T>(x: $ReadOnly<S>): $ReadOnly<T> {
  return x; // error S incompatible with T
}

function test3<S: { a: mixed }, T: { a: mixed }>(x: Pick<S, "a">): Pick<T, "a"> {
  return x; // error S incompatible with T
}

function test4<S, T>(x: Partial<S>): Partial<T> {
  return x; // error S incompatible with T
}

function test5<S, T>(x: Required<S>): Required<T> {
  return x; // error S incompatible with T
}
