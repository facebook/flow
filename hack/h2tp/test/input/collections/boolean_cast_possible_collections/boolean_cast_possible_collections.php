<?hh
class Foo {
  public $prop;
  public static $s_prop;
  public function __construct() {
    $this->prop = Vector {};
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
  echo("Expression Type $type: ");
  echo($b ? "Truthy\n" : "Falsy\n");
}
function test_all_expression_types() {
  $v = Vector {};
  $f = new Foo();
  test_cast("clone", (bool)(clone $v));
  test_cast("obj_get", (bool) $f->prop);
  test_cast("array_get", (bool)(array($v)[0]));
  test_cast("class_get", (bool)Foo::$s_prop);
  test_cast("cast", (bool)(object)$v);
  test_cast("cast", (bool)(array)$v);
  for ($v_in = Vector {1}, $i = 1; $i > 0, $v_in; $i++, $v_in->pop()) {
    echo("in here at $i\n");
    if ($i > 5) break;
  }
  test_cast("Eif", (bool)(true ? $v : $f));
  test_cast("unsafe_expr", (bool) /* UNSAFE_EXPR */ $v);
  test_cast("call", (bool) Foo::s_func());
  test_cast("Collection", (bool) Vector {});
  test_cast("New", (bool) new Foo());
  test_cast("New", (bool) new Vector(array()));
  test_cast("Eq", (bool) $y = false);
  test_cast("Eq", (bool) $z = $f->i_func());
  echo($z."\n");
}
test_all_expression_types();
