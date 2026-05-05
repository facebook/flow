//@flow

{
  class C extends (42 as any as D).f(x => 42) {}

  class D {
    f(x: unknown): any {
      return C;
    }
  }
}

{
  type Ref = { children: Array<Node> };
  declare const referencedInClassExtends: Ref;
  declare function f(v: unknown): unknown;
  // Node does not depend on f and referencedInClassExtends
  class Node extends (f(referencedInClassExtends) as any) {}
}
