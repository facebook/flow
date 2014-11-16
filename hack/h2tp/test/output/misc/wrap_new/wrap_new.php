<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class C {
  public function foo() {
    return 5;
  }
}
class D {
  public $state = "";
  public function __construct($foo, $bar) {
    return $this->state = $foo.$bar;
  }
}
echo (\hacklib_id(new C())->foo());
echo (\hacklib_id(new D("rin tin ", "tin"))->state);
echo ("\n");
