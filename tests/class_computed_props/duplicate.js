const s = 'foo' as const;

class C {
  foo: number = 1;
  [s]: string = 'x'; // ERROR: `foo` already declared in this class
}

declare const c: C;
c.foo; // OK: the member still exists despite the duplicate-declaration error
