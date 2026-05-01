class Variance<out Out, in In> {
  foo(x: Out): Out { return x; }
  bar(y: In): In { return y; }
}

class A { }
class B extends A { }

function subtyping(
  v1: Variance<A,B>,
  v2: Variance<B,A>
) {
  v1 as Variance<B,A>; // error on both targs (A ~/~> B)
  v2 as Variance<A,B>; // OK for both targs (B ~> A)
}

class PropVariance<out Out, in In> {
  inv1: Out; // error
  inv2: In; // error
  writeonly co1: Out; // error
  writeonly co2: In; // ok
  readonly con1: Out; // ok
  readonly con2: In; // error

  inv_dict1: {[k:string]: Out}; // error
  inv_dict2: {[k:string]: In}; // error
  readonly co_dict1: {readonly [k:string]: Out}; // ok
  readonly co_dict2: {readonly [k:string]: In}; // error
  readonly con_dict1: {writeonly [k:string]: Out}; // error
  readonly con_dict2: {writeonly [k:string]: In}; // ok
}
