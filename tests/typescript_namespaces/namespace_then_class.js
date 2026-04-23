// `declare namespace X { type T = ... }` BEFORE `declare class X {}` —
// the namespace's type members must merge into the class's type side
// (`X.T` resolves to `string`), not be lost. This is the lru-cache shape.
declare namespace M { type T = string; }
declare class M {}

'' as M.T; // ok
1 as M.T; // err: number ~> string
new M(); // ok: M is a constructor on the value side
