// @flow

class Base {}
class Middle extends Base {}
class Child extends Middle {}

class C<T: Middle> {
  meth(a: T): T {
    return a;
  }
}


var a: C = new C();

a.meth(new Middle());
a.meth(new Child());
a.meth(42); // Error: number ~> Middle
a.meth(new Base()); // Error: Base ~> Middle
