class A {
  m(): void {}
  static staticM(): void {}
}

interface I {
  anotherM(): void;
  static anotherStaticM(): void;
}

function fn(a: I & A): void {
  a.m();
  a.anotherM();
  a.constructor.staticM();
  a.constructor.anotherStaticM();
  var ap = new a.constructor();
  ap.m();
  ap.anotherM();
  ap.constructor.staticM();
  ap.constructor.anotherStaticM();
}

interface J {
  yaM(): void;
  static yaStaticM(): void;
}

function gn(Ap: Class<I> & Class<A> & J): void { // Forgot `Class<.>` on `J`
  Ap.staticM();
  Ap.anotherStaticM();
  Ap.yaM();
  Ap.constructor.yaStaticM(); //OK? It seems more okay for a signature like `gn(ap: K & L & Class<A>): void`
  var a = new Ap(); // Here, J gets silently dropped from the intersection. OK?
  a.m();
  a.anotherM();
  a.constructor.staticM();
  a.constructor.anotherStaticM();
  a.yaM(); //NG
  a.constructor.yaStaticM(); //NG
}

class JLess extends A {
  anotherM(): void {}
  static anotherStaticM(): void {}
}
var jLess = new JLess;
fn(jLess);
gn(JLess); //NG: False positive: Where's the "missing `yaStaticM` property" error? (Probably tied to the silent `J` dropping--wait for hard spec on that behavior)

class Full { // Forgot `extends A`
  anotherM(): void {}
  yaM(): void {}
  static anotherStaticM(): void {}
  static yaStaticM(): void {}
}
var full = new Full;
fn(full);
gn(Full);

class AlmostJ extends A {
  anotherM(): void {}
  yaM(): void {}
  yaStaticM(): void {}
  static anotherStaticM(): void {}
}
var almostJ = new AlmostJ;
fn(almostJ);
gn(AlmostJ); //NG: False positive: Where's the "missing `yaStaticM` property" error? ...
