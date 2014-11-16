<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class C {
  public static function f($foo, $whatever) {
    $x = ((int) (5.3 + 2.8)) + 20;
    echo ($x);
  }
}
class D {
  public static function foo($x, $y) {
    return $x->map(
      function ($i) use ($y) {
        return $y[$i];
      }
    );
  }
}
C::f(array(), null);
