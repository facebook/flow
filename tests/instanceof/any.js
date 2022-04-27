// @flow

class A {}
class P<X> {
  n: number;
  x: X;
}

declare var a: any;

if (a instanceof A) {
    (a: empty); // error A ~> empty
}
if (a instanceof P) {
    (a: empty); // error P ~> empty
    (a.n: empty); // error number ~> empty
    (a.x: empty); // ok X instantiated to any
}
