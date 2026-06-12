declare opaque type A;
declare opaque type B: A;

class C<out Out, in In extends Out = Out> {}

declare const x: C<B>;
x as C<A>; // error: A ~> B in default-expanded type
