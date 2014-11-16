<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class C {
  public function foo() {
    $x = 1 + (2 * (3 - (2 / 5)));
    $y = (-(++$x)) % (+10);
    $x -= $y++;
    $y /= --$z;
    $b = "hi"." hello";
    $c .= "hey there";
    $n = ($x--) | ($y & (($z ^ ((~$x) >> 2)) << 3));
    $yes =
      (((((boolean) \hacklib_cast_as_boolean($x)) && ($x > $y)) ||
        (!($x >= $z))) ^
       ($z < $y)) ||
      ($z <= $x);
    $maybe =
      \hacklib_equals($x, $y) &&
      ($x === $y) &&
      \hacklib_not_equals($z, $x) &&
      ($z !== $y);
  }
}
