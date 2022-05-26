// @flow

function test1() {
  declare function f<T>(f: (string) => T): T;
  // We should be able to infer that c is string.
  const c = f((a) => a);
  (c: string);
}
