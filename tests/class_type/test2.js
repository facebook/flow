class A {}
class B extends A {}

class K {
  static gn() {
    return this;
  }
}

class L extends K {
  static gn() { // NG Bug: False positive.
    return L;
  }
}

class W {
  fn(): Class<A> {
    return B;
  }
  static fn(): Class<A> {
    return B;
  }
  static gn(): Class<W> {
    return W;
  }
}

class X extends W {
  static gn(): Class<this> {
    return this;
  }
}

class Y extends X {
  static gn(): Class<Y> { //NG
    return Y;
  }
}

var a: Class<W> = X.gn();
