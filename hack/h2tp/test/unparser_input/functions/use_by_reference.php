<?hh
$a = "do";
$b = "re";
$fn = function (&$x) use (&$b) {
  $x = "mi";
  $b = "fa";
};
$fn($a);
echo("the param $a\n");
echo("the used $b\n");
class C {
  public function mk_lm(&$y, $toWhat) {
    return function () use (&$y, $toWhat) {
      $y = $toWhat;
    };
  }
}
$c = new C();
$d = "awake";
$sleep = $c->mk_lm($d, "asleep");
echo($d."\n");
$sleep();
echo($d."\n");
