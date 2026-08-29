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

n.method() as number;
n.method() as string; // error (number incompatible with string)
s.method() as string;
s.method() as number; // error (number incompatible with string)

n.method.bind(s); // errors: type argument and receiver mismatch
s.method.bind(n); // errors: type argument and receiver mismatch


declare class B<T> {
    foo<X>(this: B<X>) : void
}
