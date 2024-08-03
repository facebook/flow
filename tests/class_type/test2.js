// A function to typecheck values against their types. Covariance of Class<.>
// makes it useless in such a function (when limited to classes and instances),
// since everything can be trivially satisfied by going to `mixed`.
declare function check<C>(cls: C, inst: C extends Class<infer I> ? I : empty): void;

class A { }
class B extends A { }
class C { }

check(B, new A);
check(A, new B);
check(C, new A);
check(C, new B);
check(B, new C);
