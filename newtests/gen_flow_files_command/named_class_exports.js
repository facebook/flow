// @flow

export class Base<A, B, C> {
  static baseStaticMethod(a: number, b: string) { return a; }
  static overriddenStaticMethod(a: {b: number, c: number}) { return a.b + a.c; }

  // Testing infinite type recursion
  baseInst: Base<number, string, mixed>;

  // Testing forward references
  childInst: Child<string, number>;

  baseMethod(a: number, b: string) { return a; }
  overriddenMethod(a: {b: number, c: number}) { return a.b + a.c; }
};

export class Child<A, B> extends Base<A, B, mixed> {
  static overriddenStaticMethod(a: {b: number}) { return a.b; }

  notExported: NotExportedUsed<number>;
  overriddenMethod(a: {b: number}) { return a.b; }
}

class NotExportedUsed<T> {
  map<U>(f: (x:T) => U): NotExportedUsed<U> {
    return new NotExportedUsed();
  };
}
class NotExportedNotUsed {}
