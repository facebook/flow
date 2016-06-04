//@flow

class A {}

interface I {
  m1(): string;
  static sm1(): number;
}

function fn(): Class<A> & Class<I> {
  return class K extends A {
    m1() { return "a string"; }
    static sm1() { return 1; }
  }
}

function gn(): Class<A> & Class<I> {
  return class K extends A {}
}

var X = fn();
var x = new X();

var Y = gn();
