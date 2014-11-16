<?php
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
      (((((boolean) $x) && ($x > $y)) || (!($x >= $z))) ^ ($z < $y)) ||
      ($z <= $x);
    $maybe = ($x == $y) && ($x === $y) && ($z != $x) && ($z !== $y);
  }
}
