<?hh
class C {
  public function foo() {
    return "foo";
  }
}
interface I {
  require extends C;
  public function bar();
}
trait t {
  require extends C;
  require implements I;
}
class D extends C implements I {
  use t;
  public function bar() {
    return "bar";
  }
}
