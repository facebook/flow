<?hh
function testIterator($b) {
  for ($i=0; $i < 5; $i++) {
    echo ($b->current()."\n");
    $b->next();
  }
}
trait FakeIterator {
  public function current() {
    return 10;
  }
  public function key() {
    return "booyah";
  }
  public function next() {
  }
  public function rewind() {
  }
  public function valid() {
    return true;
  }
}
class Foo implements Iterator {
  use FakeIterator;
}
class Bar extends Foo implements HH\Iterator {
}
class Baz implements \Iterator {
  use FakeIterator;
}
testIterator(new Foo());
testIterator(new Bar());
testIterator(new Baz());
