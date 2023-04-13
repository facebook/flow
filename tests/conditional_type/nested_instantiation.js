{
  declare function poly<T>(T extends number ? number : string, T): T;
  poly(3, 3); // ok
}

{
  declare function poly<T>(T extends number ? number : string): T;
  poly(3); // underconstrained
}

{
  declare function id<T>(Array<T>): Array<T>;
  type Id<T> = T extends Array<infer E> ? Array<E> : T;
  declare var a: Id<Array<string>>;
  id(a); // ok
}
