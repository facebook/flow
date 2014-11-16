<?hh
trait Foo {
  public function bar() {
    return $this->baz($this->garply." in mah trait\n");
  }
  public $garply = null;
  protected abstract function baz($str);
}
trait Corge {
  public static function qux() {
    return "some static stuff\n";
  }
}
class Grault {
  use Foo, Corge;
  protected function baz($str) {
    echo ($str);
  }
}
echo(Grault::qux());
$a = new Grault();
$a->garply = "violating your encapsulation";
$a->bar();
