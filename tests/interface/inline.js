type A = interface { p: number }
type B = interface extends A { q: number }

({p: 0} as A); // OK
({p: "bad"} as A); // error: string ~> number

({p: 0, q: 0} as B); // OK
({p: "bad", q: 0} as B); // error: string ~> number
({p: 0, q: "bad"} as B); // error: string ~> number
({q: 0} as B); // error: missing property p
({p: 0} as B); // error: missing property q

interface J { p: number }
interface K { q: string }
function f(o: interface extends J, K {}) {
  o.p as empty; // error: number ~> empty
  o.q as empty; // error: string ~> empty
  o.z; // error: undeclared property z

  // unlike named interfaces, inline interfaces don't have a `name` property
  o.name;
}

// unlike named interfaces, inline interfaces are checked for polarity
type C<+T> = interface { p: T }

interface Base { p: number }
type Derived = interface extends Base {
  p: string, // error: string ~> number
}

{
  declare const x: interface {foo: number};
  x as interface {foo: string}; // ERROR
}
