/* @flow */

import { A } from "./A.js"

class B extends A {
  m(n) { return n; }
  static static_m(s) { return s; }
}

let b1 = new B;
let b2: A = new B;
let b3: B = new B;
let B4: Class<A> = A; //ng
let B5: Class<B> = B;

let n1 = b1.m(1);
let s1 = B5.static_m("two");
