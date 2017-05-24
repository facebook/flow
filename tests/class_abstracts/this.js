/* @flow */

class A {
  abstract clone(): this;
  abstract static build(): this;
}

class B extends A {
  clone(): this {
    return new this.constructor;
  }
  static build(): this {
    return new this;
  }
}

let X: Class<A> = B;
let x1: A = new X;
let x2: A = x1.clone();
let x3: A = X.build();
