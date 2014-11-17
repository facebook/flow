<?hh
class C {
  private function stuff($a, $b) {
    return $a + $b;
  }
  public function foo($x) {
    $bar = $y ==> $this->stuff($x, $y);
    return $bar(33);
  }
  public function bar($x) {
    $zeta = new_model($x);
    return $y ==> $this->stuff($y, $zeta);
  }
}
function lam($b): int {
  $fn = $a ==> $a + $b;
  $gn = $a ==> {
    $c = $b * 20;
    return $a + $b;
  };
  return $fn(10) + $gn(10);
}
var_dump(lam(42));
$c = new C();
var_dump($c->foo(20));
