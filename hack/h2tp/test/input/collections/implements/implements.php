<?hh
namespace HH {
  interface Unrelated {}
}
namespace {
  function testKeys($c) {
    echo(implode("," ,$c->keys())."\n");
  }
  interface Unrelated {}
  trait IterMethods {
    public function mapWithKey($callback) {
      throw new Exception("not implemented");
    }
    public function filterWithKey($callback) {
      throw new Exception("not implemented");
    }
    public function keys() {
      return array("fee", "fi", "fo", "fum");
    }
    public function getIterator() {
      throw new Exception("not implemented");
    }
  }
  class Foo implements KeyedIterable<string, int>, Unrelated{
    use IterMethods;
  }
  class Bar implements HH\KeyedIterable<string, int>, HH\Unrelated {
    use IterMethods;
  }
}
namespace blah {
  class Baz implements \HH\KeyedIterable<string, int>, \HH\Unrelated {
    use \IterMethods;
  }
  testKeys(new \Foo());
  testKeys(new \Bar());
  testKeys(new Baz());
}
