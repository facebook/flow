{
  declare function poly<T>(a: T extends number ? number : string, b: T): T;
  poly(3, 3); // ok
}

{
  declare function poly<T>(x: T extends number ? number : string): T;
  poly(3); // underconstrained
}

{
  declare function id<T>(x: Array<T>): Array<T>;
  type Id<T> = T extends Array<infer E> ? Array<E> : T;
  declare var a: Id<Array<string>>;
  id(a); // ok
}
