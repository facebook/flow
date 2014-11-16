<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class Foo {
  public function bar() {
    yield 5;
    return;
  }
  public static function baz() {
    if (false) {
      yield false;
    }
    return;
  }
}
foreach (Foo::baz() as $v) {
  echo ($v."\n");
}
echo ("break\n");
$f = new Foo();
foreach ($f->bar() as $v) {
  echo ($v."\n");
}
