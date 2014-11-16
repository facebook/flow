<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class A {
  public function me() {
    return get_class($this);
  }
}
class B extends A {}
class Foo {
  private $t;
  public function __construct($t) {
    $this->t = $t;
  }
  public function getMe() {
    return $this->t->me();
  }
}
function printAFoo($f) {
  echo ($f->getMe()."\n");
}
function callWithFooA($f) {
  printAFoo($f);
}
function callWithFooB($f) {
  printAFoo($f);
}
callWithFooA(new Foo(new A()));
callWithFooB(new Foo(new B()));
interface Bar {
  public function gimme();
}
trait Baz {
  public function gimme() {
    throw new Exception("cheat");
  }
  protected static final function hacklib_initialize_statics_Baz() {
  }
}
class DunDunDun extends Foo implements Bar {
  use Baz;
  public static function hacklib_initialize_statics() {
    self::hacklib_initialize_statics_Baz();
  }
}
DunDunDun::hacklib_initialize_statics();
callWithFooA(new DunDunDun(new A()));
