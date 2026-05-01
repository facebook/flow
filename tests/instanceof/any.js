class A {}
class P<X> {
  n: number;
  x: X;
}

declare var a: any;

if (a instanceof A) {
    a as empty; // error A ~> empty
}
if (a instanceof P) {
    a as empty; // error P ~> empty
    a.n as empty; // error number ~> empty
    a.x as empty; // ok X instantiated to any
}
