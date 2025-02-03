declare class D extends C {}

declare class C {
  m(this1: C): this is D;
}
