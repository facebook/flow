{
  // Basic functionality test
  type Keys = 'foo' | 'bar';
  type Mapped = {[key in Keys]: number};

  declare const mapped: Mapped;
  (mapped.foo: number); // OK!
  mapped.foo = 3; // OK!


  (mapped: empty); // ERROR!
  (mapped: {foo: empty, bar: empty}); // ERROR 2x
  (mapped: {foo: number, bar: number}); // OK!
}

{
  // Leftover types are converted to indexers
  type WithLeftovers = {[key in 'foo' | number]: number};
  declare const withLeftovers: WithLeftovers;
  (withLeftovers: {[number]: number, foo: number}); // OK!
  (withLeftovers: {[number]: string, foo: string}); // ERROR 2x
}

{
  // Key types must be string | number | symbol, otherwise error on the
  // definition.
  // TS Reference: https://www.typescriptlang.org/play?ts=5.0.4#code/C4TwDgpgBAsghmSATKBeKBvA2gawiKASwDsoAjAewoBsI5iBdALimIFcBbMiAJwF8A3EA
  type BadKeys = {[key in boolean]: number}; // ERROR!
  declare const badKeys: BadKeys;
  (badKeys: any);
}

{
  // Tests using mapped types in output positions for implicit instantiation
  declare function keyMirror<T>(...$ReadOnlyArray<T>): {[key in T]: key};

  let keyMirroredObject = keyMirror('foo', 'bar', 'baz');
  (keyMirroredObject: {foo: 'foo', bar: 'bar', baz: 'baz'});

  (keyMirroredObject: {foo: 'bar', bar: 'baz', baz: 'foo'}); // ERROR x3
}

{
  // Tests using mapped types in input positions for implicit instantiation
  declare function ObjWithKeys<T>(x: {[key in T]: number}, ...$ReadOnlyArray<T>): {[key in T]: number};
  const noContext = ObjWithKeys({foo: 3, bar: 3}, 'foo', 'bar'); // OK!
  (noContext: {foo: number, bar: number}); // OK!
  (noContext: {foo: empty, bar: empty, baz: number}); // ERROR 3x
  const withReturnAnnot: {foo: number, bar: number} = ObjWithKeys({foo: 3, bar: 3}, 'foo', 'bar'); // OK!
}
