//@flow

// After refinement, the type of x should be narrowed to number,
// but the provider_type should remain number | string.
let x: number | string = 42;
if (typeof x === 'number') {
  x;
}

declare opaque type F<X> super X extends X;

declare const f: F<number => number>;
f;
f(10);
