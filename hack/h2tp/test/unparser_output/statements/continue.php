<?php
class Foo {
  public static function bar() {
    for ($x = 1; $x < 1000; $x++) {
      if ($x < 999) {
        continue;
      }
      echo ("I'm here\n");
    }
  }
}
Foo::bar();
