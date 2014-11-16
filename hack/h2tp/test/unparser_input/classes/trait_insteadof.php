<?hh
trait A {
  public function sayHello() {
    echo 'Hello from A';
  }
}
trait B {
  public function sayHello() {
    echo 'Hello from B';
  }
}
class C {
  use A, B {
    A::sayHello insteadof B;
  }
}
