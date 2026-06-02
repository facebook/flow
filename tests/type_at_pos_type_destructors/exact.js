// @flow

type A = { x: number, ... };
type B = $Exact<A>;
//   ^
declare const a: $Exact<A>;
//            ^
declare const b: $Exact<B>;
//            ^
declare const c: $Exact<{ p: number, ... }>;
//            ^

function foo<X>(x: $Exact<X>) {
//              ^
  var y: $Exact<X>;
//    ^
}

declare const e: $Exact<$Exact<A>>;
//            ^

class C {}
declare const f: $Exact<Class<C>>;
//            ^

type P<X> = $Exact<{ m: (x: X) => void, ...}>;
//   ^
type Q<X> = $Exact<P<X>>;
//   ^

type R = $Exact<{ readonly f: number, ... }>;
declare const r: {response: R};
const d = r.response;
function bar({d}:{d: R, ...}) {}
//            ^
bar({d});
