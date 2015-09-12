class A<X> { }
new A; // OK, implicitly inferred type args
class B extends A { } // OK, same as above

function foo(b): A { // assuming A<any>
  return b ? (new A: A<number>): (new A: A<string>);
}

function bar(): A<*> { // error, * can't be {} and {x: string} at the same time
  return (new A: A<{}>) || (new A: A<{x: string}>);
}
