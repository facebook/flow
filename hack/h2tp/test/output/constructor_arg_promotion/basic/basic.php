<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class C {
  private $bar;
  private function foo() {
  }
  protected $foo;
  public $zan;
  private $zee;
  public function __construct($foo, $bar, $zan, $zee) {
    $this->foo = $foo;
    $this->zan = $zan;
    $this->zee = $zee;
  }
}
function yo() {
  $x = false;
  new C("hello", 5, $x, 1);
}
