<?hh
interface Singer {
  public function sing();
}
trait PopSinger implements Singer {
  public function sing() {
    echo "Imagine";
  }
}
class Foo {
  use PopSinger;
}
(new Foo())->sing();
