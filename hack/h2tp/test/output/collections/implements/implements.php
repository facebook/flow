<?php
namespace HH {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  interface Unrelated {}
}
namespace {
  function testKeys($c) {
    echo (implode(",", $c->keys())."\n");
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
    protected static final function hacklib_initialize_statics_IterMethods() {
    }
  }
  class Foo implements \HH\KeyedIterable, Unrelated {
    use IterMethods;
    public static function hacklib_initialize_statics() {
      self::hacklib_initialize_statics_IterMethods();
    }
  }
  Foo::hacklib_initialize_statics();
  class Bar implements \HH\KeyedIterable, HH\Unrelated {
    use IterMethods;
    public static function hacklib_initialize_statics() {
      self::hacklib_initialize_statics_IterMethods();
    }
  }
  Bar::hacklib_initialize_statics();
}
namespace blah {
  class Baz implements \HH\KeyedIterable, \HH\Unrelated {
    use \IterMethods;
    public static function hacklib_initialize_statics() {
      self::hacklib_initialize_statics_IterMethods();
    }
  }
  Baz::hacklib_initialize_statics();
  testKeys(new \Foo());
  testKeys(new \Bar());
  testKeys(new Baz());
}
