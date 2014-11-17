<?hh
class Foo<T> {}
function foo(Vector<int> $x) {}
function bar() : Foo<Iterable> {
  return new Foo();
}
function baz(?Map $y) {}
function qux((function(Vector):void) $fn) {}
function quux((function():Traversable<string>) $fn) {}
function garply():(string, ?ImmSet) {
  return tuple('', null);
}
newtype grault = shape('x' => Pair);
class Blue extends Vector<string> {}
class Red implements Collection<string> {}
trait Green {
  require implements KeyedIterable<string, int>;
}
