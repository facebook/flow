const o = { k: 'mem' } as const;
declare const bad: string;
const o2 = { k: bad };

class C {
  [o.k]: number = 1; // member-expression key resolves to `mem`
  [o2.k]: string = 'x'; // ERROR: member key is a general string, not a literal
}

declare const c: C;

c.mem as number; // OK
c.mem as string; // ERROR: number is not string
