// @flow

function test1() {
  declare function f<T>(f: (string) => T): T;
  // We should be able to infer that c is string.
  const c = f((a) => a);
  (c: string);

  let f2: (string) => string = (a) => (a: string); // ok
  f2 = (b) => (b: string); // ok
}
