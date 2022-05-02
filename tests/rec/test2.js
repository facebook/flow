var a = []; // Array<X> ~> a
function bar() {
  a = a.concat([]); // terminate despite expanding types:
  // a ~> .concat(Array<Y>)
  // Array<X> ~> .concat(Array<Y>)
  // Array<X|Y> ~> a
  // Array<X|Y> ~> .concat(Array<Y>)
  // Array<X|Y|Y> ~> a
};

class A<X> {
  x: A<A<X>>;
}
var a_ = new A;
function foo0() {
  // $FlowExpectedError in env-mode ssa
  a_ = a_.x; // terminate despite expanding types. This is not an issue in constrained writes
}

class D<X> { }
class B<X> extends D<X> { }
class C<X> extends B<X> { }
((new C: C<number>): D<string>) // error: number ~/~ string
