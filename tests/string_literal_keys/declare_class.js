// String literal property names in declare class

// Field with annotation
declare class A {
  "foo": string;
  "aria-label": string;
}
{
  declare const a: A;
  a.foo as string; // OK
  a["aria-label"] as string; // OK

  a.foo as empty; // ERROR
}

// Static field
declare class B {
  static "bar": number;
}
{
  B.bar as number; // OK
  B.bar as empty; // ERROR
}

// Method
declare class C {
  "baz"(): string;
}
{
  declare const c: C;
  c.baz() as string; // OK
  c.baz() as empty; // ERROR
}

// Optional field
declare class D {
  "opt"?: boolean;
}
{
  declare const d: D;
  d.opt as boolean | void; // OK
}

// String literal "constructor"
declare class F {
  "constructor"(x: string): void;
}
{
  const f = new F("hello"); // OK
  new F(42); // ERROR
}

// Getter and setter
declare class E {
  get "prop"(): number;
  set "prop"(value: number): void;
}
{
  declare const e: E;
  e.prop as number; // OK
  e.prop as empty; // ERROR
}
