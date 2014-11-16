<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class C {
  private function stuff($a, $b) {
    return $a + $b;
  }
  public function foo($x) {
    $bar = function ($y) use ($x) {
      return $this->stuff($x, $y);
    };
    return $bar(33);
  }
  public function bar($x) {
    $zeta = new_model($x);
    return function ($y) use ($zeta) {
      return $this->stuff($y, $zeta);
    };
  }
}
function lam($b) {
  $fn = function ($a) use ($b) {
    return $a + $b;
  };
  $gn = function ($a) use ($b) {
    $c = $b * 20;
    return $a + $b;
  };
  return $fn(10) + $gn(10);
}
var_dump(lam(42));
$c = new C();
var_dump($c->foo(20));
