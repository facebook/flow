// @flow

class Base { }

class AAA<X: Base> {
  xxx: X;
  mm() {}
}

class BBB<XXXX: Base> extends AAA<XXXX> {

  constructor() {
    super();
  }

  mm() {
    this.xxx;
    super.mm();
  }

  nn() {
    this.mm();
  }
}
