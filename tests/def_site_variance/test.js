class Variance<+Out,-In> {
  foo(x: Out): Out { return x; }
  bar(y: In): In { return y; }
}

class A { }
class B extends A { }

function subtyping(
  v1: Variance<A,B>,
  v2: Variance<B,A>
) {
  (v1: Variance<B,A>); // error on both targs (A ~/~> B)
  (v2: Variance<A,B>); // OK for both targs (B ~> A)
}
