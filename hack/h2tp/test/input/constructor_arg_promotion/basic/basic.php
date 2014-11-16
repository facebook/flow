<?hh

class C {
  private $bar;

  private function foo() {}

  public function __construct(
    protected string $foo,
    int $bar,
    public bool $zan,
    private $zee
  ): @void {
  }
}

function yo() {
  $x = false;
  new C("hello", 5, $x, 1);
}
