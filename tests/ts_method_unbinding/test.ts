class A {
  m(): void {}
  n = (): void => {}
}

interface I {
  m(): void;
  n: () => void;
}

declare const a: A;
declare const i: I;

// Unbinding class methods is OK in .ts (would be method-unbinding error in .js)
a.m; // OK
a.m satisfies () => void; // OK
a.m satisfies empty; // ERROR: proves type is not any

const {m} = a; // OK
m satisfies () => void; // OK
m satisfies empty; // ERROR: proves type is not any

if (a.m) {} // OK

// Unbinding interface methods is OK in .ts
i.m; // OK
i.m satisfies () => void; // OK
i.m satisfies empty; // ERROR: proves type is not any

const {m: im} = i; // OK
im satisfies () => void; // OK
im satisfies empty; // ERROR: proves type is not any

// `this` type is unbound (becomes any) — matches TypeScript semantics
class B {
  self(): this { return this; }
}
declare const b: B;
const {self} = b; // OK
self satisfies () => B; // OK — any is compatible with B
self satisfies empty; // ERROR: function type is not empty

// Calling unbound methods should work in .ts
const f = a.m;
f(); // OK — should not error in .ts

const {m: am} = a;
am(); // OK — should not error in .ts

// Arrow function properties are always OK (unchanged behavior)
a.n; // OK
i.n; // OK

// Calling methods is always OK (unchanged behavior)
a.m(); // OK
i.m(); // OK
