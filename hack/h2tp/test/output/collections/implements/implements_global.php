<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function testIterator($b) {
  for ($i = 0; $i < 5; $i++) {
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
  protected static final function hacklib_initialize_statics_FakeIterator() {
  }
}
class Foo implements \HH\Iterator {
  use FakeIterator;
  public static function hacklib_initialize_statics() {
    self::hacklib_initialize_statics_FakeIterator();
  }
}
Foo::hacklib_initialize_statics();
class Bar extends Foo implements \HH\Iterator {}
class Baz implements \Iterator {
  use FakeIterator;
  public static function hacklib_initialize_statics() {
    self::hacklib_initialize_statics_FakeIterator();
  }
}
Baz::hacklib_initialize_statics();
testIterator(new Foo());
testIterator(new Bar());
testIterator(new Baz());
