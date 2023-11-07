//@flow

function f<X: {...}>(ex: $Exact<X>, x: X) {
  ex as X;
  ex as $Exact<X>;
  x as $Exact<X>; // nope
  42 as any as {||} as $Exact<X>; // nope
}
