{
  type InferTypeOverlapGenerics<T> = {foo: T, bar: number} extends {foo: T, bar: infer T} ? T : boolean;
  const num = (1: InferTypeOverlapGenerics<string>); // ok
  (num: empty); // error: number ~> empty
}

{
  declare function flat<T>(array: Array<T>): Array<T extends $ReadOnlyArray<infer E> ? E : T>
  // Regression test for extraneous free vars detection in conditional type
  const doubleFlat: $ReadOnlyArray<number> = flat([1].map(n => flat([n]))); // ok
}
