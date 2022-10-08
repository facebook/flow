class A<X> { }
new A<mixed>;
class B extends A { } // OK, same as above

function foo(b: boolean): A<any> { // ok but unsafe, caller may assume any type arg
  return b ? (new A: A<number>): (new A: A<string>);
}
