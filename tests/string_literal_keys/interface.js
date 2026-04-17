// String literal property names in interface

// Field with annotation
interface A {
  "foo": string;
  "aria-label": string;
  "on:click": () => void;
}
{
  declare const a: A;
  a.foo as string; // OK
  a["aria-label"] as string; // OK
  a["on:click"] as () => void; // OK

  a.foo as empty; // ERROR
}

// Optional field
interface B {
  "opt"?: number;
}
{
  declare const b: B;
  b.opt as number | void; // OK
}

// Method
interface C {
  "bar"(): string;
}
{
  declare const c: C;
  c.bar() as string; // OK
  c.bar() as empty; // ERROR
}

// Getter and setter
interface D {
  get "baz"(): number;
  set "baz"(value: number): void;
}
{
  declare const d: D;
  d.baz as number; // OK
  d.baz as empty; // ERROR
}

// Quoted valid identifiers (common TS pattern for event maps)
interface EventMap {
  "abort": string;
  "error": number;
  "message": string;
}
{
  declare const e: EventMap;
  e.abort as string; // OK
  e.error as number; // OK
  e.message as string; // OK
  e.message as empty; // ERROR
}

// Interface extends with string literal keys
interface Base {
  "x": number;
}
interface Child extends Base {
  "y": string;
}
{
  declare const c: Child;
  c.x as number; // OK
  c.y as string; // OK
}
