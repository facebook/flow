<?hh
class C {
  public function foo() {
    return "hello ";
  }
}
interface I {
  public function bar();
}
trait T {
  require extends C;
  require implements I;
  public function baz() {
    return $this->foo().$this->bar();
  }
}
class D extends C implements I {
  use T;
  public function bar() {
    return "world\n";
  }
}