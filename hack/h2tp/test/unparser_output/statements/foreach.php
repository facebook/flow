<?php
class Foo {
  public static function bar() {
    foreach (array("zip", "zap") as $x) {
      echo ($x."\n");
    }
  }
}
Foo::bar();
