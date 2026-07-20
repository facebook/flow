const m = 'go' as const;
const k = 3 as const;

class C {
  [m](x: number): number { return x; }
  [k](): string { return 'y'; }
  static [m](): boolean { return true; }
}

declare const c: C;

c.go(1) as number; // OK
c[m](1) as number; // OK
c[3]() as string; // OK
c['3']() as string; // OK
C.go() as boolean; // OK

c.go(1) as string; // ERROR: number is not string
c.nope(); // ERROR: method is missing
