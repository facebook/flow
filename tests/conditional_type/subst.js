{
  type InferTypeOverlapGenerics<T> = {foo: T, bar: number} extends {foo: T, bar: infer T} ? T : boolean;
  const num = (1: InferTypeOverlapGenerics<string>); // ok
  (num: empty); // error: number ~> empty
}
