declare opaque type A;
declare opaque type B: A;

class C<+Out, -In: Out = Out> {}

declare const x: C<B>;
x as C<A>; // error: A ~> B in default-expanded type
