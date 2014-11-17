<?hh
class C {
  public function foo($x, $str) {
    $y = $str ?: "Water";
    return $y.": ".($x < 100 ? ($x < 30 ? "cold" : "warm")." water" :
        ($x > 200 ? "OMG" : "steam"));
  }
}
$c = new C();
echo ($c->foo(17, "")."\n");
