class A { }
function foo(x: Class<Newable<A>>): A {
  return new x(); // OK
}

class B {
  constructor(_: any) { }
}
function bar(x: Class<Newable<B>>): B {
  return new x(); // error (too few args)
}

function baz(x: Class<B>): B {
  return new x(1); // error (non-newable `x`)
}
