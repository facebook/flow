<?hh
function foo($v = Vector {1, 2, 3}, $sep = ",") {
  $s = (array) $v;
  echo(join($sep, $s)."\n");
}
function fonz($v = Vector {}, $s = Set {}, $m = Map {}) {}
class Bar {
  public function __construct(public Iterable<string> $s = Set {"witches", "wizards"}) {
  }
}
foo();
foo(Set {1, "hi", 1}, " : ");
$b = new Bar();
foo($b->s, " - ");
foo((new Bar(Map {"x" => "y", "p" => "q"}))->s, " = ");
interface Boo {
  public function fizz($v = Vector {'a', 'b'});
}
abstract class Booze {
  abstract public function buzz($s = Set {'a'});
}
class BooImpl extends Booze implements Boo {
  public function fizz($v = Vector {1, 2}) {
    echo(join(", ", (array) $v)."\n");
  }
  public function buzz($s = Map {'a' => 'z'}) {
   echo(join(", ", (array) $s)."\n");
  }
}
function doIt(Boo $i) {
  $i->fizz();
  if ($i instanceof Booze) {
    $i->buzz();
  }
}
doIt(new BooImpl());
