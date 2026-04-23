// `declare namespace f { type T = ... }` BEFORE `declare function f(): void` —
// the namespace's type members must merge into the function via
// `wrap_function_with_namespace_types` regardless of source order.
declare namespace fn { type T = string; }
declare function fn(): void;

'' as fn.T; // ok
1 as fn.T; // err: number ~> string
fn(); // ok: fn is callable on the value side
