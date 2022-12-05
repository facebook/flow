//@flow

function remove<A>(a: A, x): A {
  (a: empty);
  (42: A);
  return a
}

const o: empty = remove<number>(1, 42);
const p: empty = remove(1, 42);

function remove_anno<A>(a: A): A {
  (a: empty);
  (42: A);
  return a
}

const q: empty = remove_anno<number>(1);
const r: empty = remove_anno(1);

function removex<A, B>(a: A): [$Rest<A, {p: B}>, B] {
  const {p, ...o} = a;
  return [o, p];
}

const [ox, px] = removex<_, number>({x: 'foo', p: 42});
(ox: {|x: string|});
(px: number);

function test_tparams_signature_scope() {
  function foo<A>(): <B: A>(b: B) => void { throw '' };
  const f = foo<number>();
  f(0);

  class Foo<A> {
    method<B: A>(b: B): void {}
  }
  const a = new Foo<number>();
  a.method(0);

  class tparams_are_scoped_per_method {
    m1<A>() {}
    m2<B: A>() {} // Error: Cannot resolve name `A`.
  }

  function tparam_in_default<X>(x: X = (42: X)) { } // Error: cannot cast 42 to X
}
