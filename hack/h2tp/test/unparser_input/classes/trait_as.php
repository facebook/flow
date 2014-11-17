<?hh
trait A {
  public function sayHello() {
    echo 'Hello from A';
  }
}
class C {
  use A {
    sayHello as private myHello;
  }
}
