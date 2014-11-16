<?php
class Foo {
  public static function bar() {
    foreach (array("zip" => 10, "zap" => 20) as $k => $v) {
      echo ($k." => ".$v."\n");
    }
  }
}
Foo::bar();
