<?php
class Foo {}
interface Bar {
  public static function bar1();
}
interface Baz {
  public function baz1();
}
interface Qux extends Baz {
  public function qux1();
}
abstract class Garply extends Foo implements Baz, Qux {
  public static function bar1() {
    return 1;
  }
  public abstract function baz1();
}
class Corge extends Garply {
  public function baz1() {
    return static::bar1();
  }
  public function qux1() {
    return $this->baz1() + 3;
  }
}
