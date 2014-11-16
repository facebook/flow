<?php
namespace foo\bar {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  trait t1 {
    public static $a = 20;
    protected static final function hacklib_initialize_statics_t1() {
    }
  }
  trait t2 {
    public static $b;
    protected static final function hacklib_initialize_statics_t2() {
      self::$b = new \HH\Vector(array("aa", "ee", "ii", "oo", "uu"));
    }
  }
  trait t3 {
    use \foo\bar\t1;
    public static $c;
    protected static final function hacklib_initialize_statics_t3() {
      self::hacklib_initialize_statics_t1();
      self::$c = \HH\Map::hacklib_new(array("uu"), array("qq"));
    }
  }
  class A {
    use t1;
    public static function hacklib_initialize_statics() {
      self::hacklib_initialize_statics_t1();
    }
  }
  A::hacklib_initialize_statics();
}
namespace foo {
  echo (bar\A::$a."\n");
  class B {
    use bar\t2;
    use \foo\bar\t3;
    public static function hacklib_initialize_statics() {
      self::hacklib_initialize_statics_t2();
      self::hacklib_initialize_statics_t3();
    }
  }
  B::hacklib_initialize_statics();
  echo (B::$c[B::$b[4]]."\n");
}
namespace {
  class C {
    use foo\bar\t3;
    public static $stuff;
    public static function hacklib_initialize_statics() {
      self::hacklib_initialize_statics_t3();
      self::$stuff = new \HH\Vector(array("uu"));
    }
  }
  C::hacklib_initialize_statics();
  echo (C::$c[C::$stuff[0]]."\n");
}
