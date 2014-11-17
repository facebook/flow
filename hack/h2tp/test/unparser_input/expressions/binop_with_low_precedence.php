<?hh

class C {
  public function foo() {
    $x = 1 xor 100;
    $y = $x and 25;
    $z = $y or 2;
  }
}
