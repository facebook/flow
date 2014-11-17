<?php
class C {
  public static function foo() {
    return "world\n";
  }
  public static $foo = "hello";
}
echo (C::$foo);
echo (C::foo());
