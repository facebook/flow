class A {}
class B {}

declare function isA(x: mixed): x is A;

declare function funA<T>(p1: (p1: mixed) => p1 is T): T;
declare function funB<T>(p1: (p1: mixed) => p1 is T, p2: mixed): p2 is T;

const x1: A = funA(isA); // okay
const x2: B = funA(isA); // error A ~> B

declare var input: mixed;
if (funB(isA, input)) {
  (input: A); // okay
  (input: B); // error A ~> B
}
