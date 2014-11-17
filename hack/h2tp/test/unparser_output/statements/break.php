<?php
class Foo {
  public static function bar() {
    for ($x = 1; $x < 1000; $x++) {
      if ($x > 2) {
        break;
      }
      echo ("Going home\n");
    }
  }
}
Foo::bar();
