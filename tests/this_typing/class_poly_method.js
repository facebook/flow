// @flow
class A<N> {
  prop : N;

  constructor(prop:N) {
      this.prop = prop;
  }

  method(this: A<N>): N {
    return this.prop;
  }

  static convert<M>(this : Class<A<N>>) {}
}

let n = new A<number>(3);
let s = new A<string>("");

(n.method() : number);
(n.method() : string); // error (number incompatible with string)
(s.method() : string);
(s.method() : number); // error (number incompatible with string)

n.method.bind(s); // TODO: error (number incompatible with string)
s.method.bind(n); //  TODO: error (number incompatible with string)


declare class B<T> {
    foo<X>(this: B<X>) : void
}
