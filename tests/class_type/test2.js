class A {}
class B extends A {}

class X {
  fn(): Class<A> {
    return B;
  }
  static fn(): Class<A> {
    return B;
  }
  static gn(): Class<X> {
    return X;
  }
}

class Y extends X {
  static gn(): Class<this> {
    return this;
  }
}

class Z extends Y {
  static gn(): Class<Z> {
    return Z;
  }
}

var K: Class<X> = Y.gn();
