//@flow

class C<X, Y> {
  x: X;
  constructor(x: X) {
    this.x = x;
  }
  get(): X {
    return this.x;
  }
  get_bad(): Y {
    return this.x;
  }
}

class D<T> {
  x: T;
  m<S>(z: S, u: T): S {
    this.x = u;
    return z;
  }
}

class E<X> extends C<X, number> {
  set(x: X): X {
    this.x = x;
    if (x) {
      return this.get_bad();
    }
    return this.get();
  }
}

class F<T> {
  arr: Array<{value: T}>;

  foo(value: T) {
    var entry: {value: T} = {value};
    this.arr[0] = entry;
  }
}

type D2<X, Y> = {p: Y};

class C2<X, Y> {
  x: {p: Y};
  foo(): D2<X, Y> {
    return this.x;
  }
}
