// @flow

type Obj = {p: string, q: number};

function test1(data: ?Obj) {
  if (data?.p == null && data?.q == null) throw '';
  // After negation, we have:
  // ==> !(!(data != null && data.p != null) && !(data != null && data.q != null))
  // ==> (data != null && data.p != null) || (data != null && data.q != null)
  // Applied: data != null && (data.p != null || (data.q != null)
  (data: Obj); // ok
}

function test2(data: ?{foo: {bar: ?Obj}}) {
  if (data?.foo.bar?.p == null && data?.foo.bar?.q == null) throw '';

  // Similar to the reasoning in test1 above, we will have:
  // data != null && data.foo.bar != null && (data.foo.bar.p != null || data.foo.bar.q != null)
  (data: $NotNullOrVoid); // ok
  (data.foo.bar: Obj); // ok
}
