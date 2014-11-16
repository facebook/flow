<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class A {
  public static $a = 25;
}
abstract class B extends A {
  private static $b;
  public static function at($i) {
    return self::$b[$i];
  }
  public static function hacklib_initialize_statics() {
    self::$b = new \HH\Vector(array("do", "re", "mi"));
  }
}
B::hacklib_initialize_statics();
class C extends B {
  public static $c;
  public static function at($i) {
    return static::$c[parent::at($i)];
  }
  public static function hacklib_initialize_statics() {
    self::$c = \HH\Map::hacklib_new(
      array("do", "re", "mi"),
      array("a deer", "a drop", "a name")
    );
  }
}
C::hacklib_initialize_statics();
echo (C::at(1)."\n");
