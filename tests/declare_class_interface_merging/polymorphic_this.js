// Phase 4 regression: an absorbed interface's `this`-returning method must
// rebind `this` to the absorbing declare class, not leak the interface's
// (now-defunct) synthetic `this` typeparam.
interface C {
  clone(): this;
}
declare class C {
  x: number;
}

declare const c: C;
const cloned = c.clone();
cloned.x as number; // OK: cloned is C, not the absorbed interface

// Reverse declaration order
declare class C2 {
  x: number;
}
interface C2 {
  clone(): this;
}
declare const c2: C2;
c2.clone().x as number; // OK

// Subclass: clone() inherited via the absorbed interface should rebind to D
declare class D extends C {
  y: string;
}
declare const d: D;
d.clone().y as string; // OK: rebinds through declare-class extends

// Multiple absorbed interfaces, both with `this`-returning methods
interface E {
  clone(): this;
}
interface E {
  twin(): this;
}
declare class E {
  z: number;
}
declare const e: E;
e.clone().z as number; // OK
e.twin().z as number; // OK
