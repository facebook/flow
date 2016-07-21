// @flow

export class Base<A, B, C> {
  // Testing infinite type recursion
  baseInst: Base<number, string, mixed>;

  // Testing forward references
  childInst: Child<string, number>;

  baseMethod(a: number, b: string) { return a; }
  overriddenMethod(a: {b: number, c: number}) { return a.b + a.c; }
};

export class Child<A, B> extends Base<A, B, mixed> {
  notExported: NotExportedUsed;

  overriddenMethod(a: {b: number}) { return a.b; }
}

class NotExportedUsed {}
class NotExportedNotUsed {}
