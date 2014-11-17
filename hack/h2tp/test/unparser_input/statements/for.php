<?hh
class Foo {
  public static function bar() {
    for ($x=1; $x<3; $x++) {
      echo("fee\n");
    }
    $y=1;
    for (; $y < 3; $y++)
      echo("fi\n");
    for ($i=1, $j=1; $i++ < 3, $i + $j < 7; ++$i, ++$j, print("fo\n"));
  }
}
Foo::bar();
