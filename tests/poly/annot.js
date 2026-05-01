class A<X> {}
new A<unknown>();
class B extends A {} // OK, same as above

function foo(b: boolean): A<any> {
  // ok but unsafe, caller may assume any type arg
  return b ? new A() as A<number> : new A() as A<string>;
}
