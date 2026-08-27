// A function type annotation with no `this` param gets an implicit top-typed
// `this`, so the contravariant receiver check in funtype subtyping actually
// applies. With the flag off that implicit `this` is `any` and every case below
// is silent.

class C {
  x: number = 0;
  m(): void {}
}

declare const c: C;

// Extracting a method whose receiver is the class instance: `unknown` is not a
// valid receiver for `C.m`, so this is rejected.
c.m as () => void; // error
c.m as (this: C) => void; // ok
c.m as (this: empty) => void; // ok
c.m as (this: unknown) => void; // error

// An interface method has an implicit top-typed `this`, so it is
// receiver-agnostic and goes anywhere.
interface I {
  m(): void;
}
declare const i: I;

i.m as () => void; // ok
i.m as (this: unknown) => void; // ok
i.m as (this: I) => void; // ok

// Plain functions and arrow values already have a top-typed receiver.
function plain(): void {}
const arrow = (): void => {};

plain as () => void; // ok
arrow as () => void; // ok

// An explicitly annotated `this` on a function value is still checked
// contravariantly against the annotation's implicit receiver.
function withThis(this: C): void {}

withThis as () => void; // error
withThis as (this: C) => void; // ok
