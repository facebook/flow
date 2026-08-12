// A Flow file constructing a global declared in a `.d.ts` lib file — the
// `lib.dom.d.ts` / `new URLSearchParams(...)` scenario.
new TsCtorGlobal() as TsCtorGlobal; // OK
new TsCtorGlobal('a=1').size as number; // OK
TsCtorGlobal.prototype as TsCtorGlobal; // OK
new TsCtorGlobal(1); // ERROR — number ~> string

new TsCtorGlobalQuoted(); // ERROR: invalid-constructor
