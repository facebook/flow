<?hh
class Foo {
  public function baz(): string {
    $p = $this->garply(); 
    if (($p != null) || /* UNSAFE_EXPR */ $p > 25) {
      return "hi";
    }
    return /* UNSAFE_EXPR */ $this->bar();
  }
  public function bar(): int {
    return 5;
  }
  public function garply(): ?string {
    return "";
  }
}
