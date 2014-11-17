<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class C {
  public function foo() {
    return "foo";
  }
}
interface I {
  public function bar();
}
trait t {
  protected static final function hacklib_initialize_statics_t() {
  }
}
class D extends C implements I {
  use t;
  public function bar() {
    return "bar";
  }
  public static function hacklib_initialize_statics() {
    self::hacklib_initialize_statics_t();
  }
}
D::hacklib_initialize_statics();
