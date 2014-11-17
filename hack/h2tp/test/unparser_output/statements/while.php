<?php
class Foo {
  public static function bar() {
    $i = 1;
    while ($i < 3) {
      echo (($i++)."\n");
    }
    while (true) {
      echo ("OHAI\n");
      break;
    }
  }
}
Foo::bar();
