<?hh
class Foo {
  public static function bar($x) {
    if ($x == 1) {
      echo("hi");
    } elseif ($x == 2) {
      echo("hello");
    } else {
      echo("uhhh");
    }
  }
}
Foo::bar(1);
Foo::bar(2);
Foo::bar(3);
