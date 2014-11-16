<?hh
  function foo() {}
  function bar($x) {}
  function baz($x, $y) {}
  async function garply() {
    return;
  }
  function qux(int $x, array $y) : string {
    return $y[$x];
  }