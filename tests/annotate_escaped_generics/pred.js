//@flow
function f3<X>(x: X): mixed %checks {
  return x;
}
function f4<X>(x: X): %checks {
  return x;
}

declare function g<X>(x: X): mixed %checks(x);
