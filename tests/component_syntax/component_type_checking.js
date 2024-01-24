// Verifies that we treat destructuring correctly and can check the body
// of a component
component InlineParams(
  foo: number,
  'special-key' as bar: number,
  baz as qux: number,
  destructured as {a}: {a: number},
) renders InlineParams {
  (foo: number); // OK
  (foo: string); // ERROR
  (bar: number); // OK
  (bar: string); // ERROR
  (baz: number); // ERROR, baz unbound
  (qux: number); // OK
  (a: number); // OK
  (a: string); // ERROR
  return null; // ERROR
}

component InlineWithRestParam(
  foo: number,
  ...rest: {foo: number}
) {
  (foo: number);
  (rest: {foo: number}); // OK
  (rest: empty); // ERROR
  return (42: any);
}

component Defaults(
  foo: number = 3, // OK
  bar: string = 3, // ERROR
  backwardsRef: typeof foo = foo, // ERROR 2x
  destructured as {a = 'str'}: {a?: number}, // ERROR
  destructuredAssigned as {b = 3}: {b?: number},
) {
  (b: number); // OK!
  return (42: any);
}

component ReactNodeDefaultReturn() {
  declare const node: React$Node;
  return node; // OK!
}

component InternalAlias() {
  type Bar = string;
  const b: Bar = 'hi'; // ok
  const c: Bar = 3; // error
  return null;
}

module.exports = { InlineParams, InlineWithRestParam, Defaults, ReactNodeDefaultReturn };
