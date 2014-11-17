<?hh
class Bar {}
type MyInt = int;
newtype Foo = Bar;

function foo(MyInt $x) : Foo {
  return Bar;
}
