class A {
  foo : number
}

let a = new A();

a as Readonly<interface {foo : number}>;

declare var x : Readonly<interface {foo : number}>;
x.foo = 3; // error as expected


class B {
  foo : number
  bar : string
  baz : boolean
}

let b = new B();

interface I {
  foo : number
}

interface J extends I {
  qux : string
}

interface K extends J {
  baz : boolean
}

b as Readonly<K>; // error
