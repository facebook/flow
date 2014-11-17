<?hh
function other_func($str) {
  echo($str."\n");
}

class Bar {
  public function bar() {
    return "zing";
  }
  public function baz($other) {
    echo($this === $other ? "me\n":"not me\n");
  }
  public function garply($n, $s, $b) {
    echo($b ? $s : $n);
    echo("\n");
  }
}

class C  {
  public function foo($x) {
    other_func("and now: ".$x?->bar());
    $x?->baz($x);
    $x?->garply(1, "hi", true);
  }
}
$c = new C();
$c->foo(new Bar());
$c->foo(null);
