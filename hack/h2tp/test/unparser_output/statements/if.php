<?php
class Foo {
  public static function bar($x) {
    if ($x == 1) {
      echo ("hi");
    }
  }
}
Foo::bar(1);
Foo::bar(2);
