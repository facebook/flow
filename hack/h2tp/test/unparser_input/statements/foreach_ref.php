<?hh
class Foo {
  public static function bar() {
    $arr = array(1, 2, 3, 4);
    foreach ($arr as &$value) {
      $value = $value * 2;
    }
    echo (join(",",$arr)."\n");
    $arr = array("k1" => 1, "k2" => 2);
    foreach ($arr as $k => &$value) {
      $value = $value * 2;
    }
    echo (join(",",$arr)."\n");
  }
}
Foo::bar();
