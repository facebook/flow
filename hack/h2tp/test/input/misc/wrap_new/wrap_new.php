<?hh

class C  {
  public function foo() {
    return 5;
  }
}

class D {
  public $state = "";

  public function __construct($foo, $bar) {
    return $this->state = $foo . $bar;
  }

}

echo((new C())->foo());
echo((new D("rin tin ", "tin"))->state);
echo("\n");
