// @flow

class X {
  #foo: number;
  constructor() {
    this?.#foo as empty;
  }
}

class Y {
  #bar: X;
  #baz: ?X;
  constructor() {
    this?.#bar as empty;
    this?.#baz as empty;
  }
}
