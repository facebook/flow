import { A } from "./A.js"

class B extends A {
  m(n) { return n; }
  static static_m(s) { return s; }
}

class C extends (B: Class<A>) {
  n(): string { return "n"; }
}

let b1 = new B;
let b2: A = new B;
let b3: B = new B;
let b4: C = new C;
let B5: Class<A> = A; //ng
let B6: Class<A> = B;
let B7: Class<B> = B;

let n1 = b1.m(1);
let s1 = B5.static_m("one");
let s2 = B6.static_m("two");
