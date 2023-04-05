{
  type ToArray<T> = T extends any ? Array<T> : empty;

  declare const unionArr: Array<string | number>;
  declare const arrUnion: Array<number> | Array<string>;

  // TODO: distributed conditional type is not implemented yet
  (arrUnion: ToArray<string | number>); // ok
  (unionArr: ToArray<string | number>); // error
}

{
  type Exclude<T, U> = T extends U ? empty : T;
  declare class A {};
  declare class B {};
  declare class C {};
  declare class D {};
  type A_or_C = Exclude<A|B|C, B>;
  (new A(): A_or_C); // ok
  (new B(): A_or_C); // error: B ~> A|C
  (new C(): A_or_C); // ok
  (new D(): A_or_C); // error: D ~> A|C
}
