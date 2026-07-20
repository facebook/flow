const s = 'foo' as const;
const n = 2 as const;

class C {
  [s]: 1 = 1;
  [n]: string = 'x';
  static [s]: boolean = true;
}

declare const c: C;

c.foo as 1; // OK
c[s] as 1; // OK
c[2] as string; // OK
c['2'] as string; // OK
C.foo as boolean; // OK

c.foo as 2; // ERROR: 1 is not 2
c[2] as number; // ERROR: string is not number

c.bar; // ERROR: property `bar` is missing
