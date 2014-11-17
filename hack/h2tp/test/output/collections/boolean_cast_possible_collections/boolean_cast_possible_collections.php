<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class Foo {
  public $prop;
  public static $s_prop;
  public function __construct() {
    $this->prop = new \HH\Vector(array());
    self::$s_prop = $this->prop;
  }
  public static function s_func() {
    return self::$s_prop;
  }
  public function i_func() {
    return $this->prop;
  }
}
function test_cast($type, $b) {
  echo ("Expression Type $type: ");
  echo (\hacklib_cast_as_boolean($b) ? "Truthy\n" : "Falsy\n");
}
function test_all_expression_types() {
  $v = new \HH\Vector(array());
  $f = new Foo();
  test_cast("clone", (bool) \hacklib_cast_as_boolean(clone $v));
  test_cast("obj_get", (bool) \hacklib_cast_as_boolean($f->prop));
  test_cast("array_get", (bool) \hacklib_cast_as_boolean(array($v)[0]));
  test_cast("class_get", (bool) \hacklib_cast_as_boolean(Foo::$s_prop));
  test_cast("cast", (bool) \hacklib_cast_as_boolean((object) $v));
  test_cast("cast", (bool) ((array) \hacklib_cast_as_array($v)));
  for ($v_in = new \HH\Vector(array(1)), $i = 1;
  $i > 0,
  \hacklib_cast_as_boolean($v_in)
  ; $i++, $v_in->pop()) {
    echo ("in here at $i\n");
    if ($i > 5) {
      break;
    }
  }
  test_cast("Eif", (bool) \hacklib_cast_as_boolean(true ? $v : $f));
  test_cast(
    "unsafe_expr",
    (bool) \hacklib_cast_as_boolean(/* UNSAFE_EXPR */ $v)
  );
  test_cast("call", (bool) \hacklib_cast_as_boolean(Foo::s_func()));
  test_cast(
    "Collection",
    (bool) (!\hacklib_id(new \HH\Vector(array()))->isEmpty())
  );
  test_cast("New", (bool) (new Foo()));
  test_cast(
    "New",
    (bool) (!\hacklib_id(new \HH\Vector(array()))->isEmpty())
  );
  test_cast("Eq", (bool) ($y = false));
  test_cast("Eq", (bool) \hacklib_cast_as_boolean($z = $f->i_func()));
  echo ($z."\n");
}
test_all_expression_types();
