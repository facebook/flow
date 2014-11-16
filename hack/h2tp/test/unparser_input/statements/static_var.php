<?hh
class Foo {
  public static function bar() {
    static $p = "20\n", $baz = 50;
    echo($baz);
    echo($p);
  }
}
Foo::bar();
